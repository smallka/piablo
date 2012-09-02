from twisted.internet.protocol import DatagramProtocol
from twisted.internet.protocol import Protocol
from twisted.internet.protocol import ClientFactory
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint

import wx
from twisted.internet import wxreactor
wxreactor.install()

# import twisted reactor *only after* installing wxreactor
from twisted.internet import reactor

from bitstring import BitStream
import pprint

import sys
import time
from bit_reader import BitReader
import log
import type_descriptor

DIRECTION_S2C = "s2c"
DIRECTION_C2S = "c2s"

dest_ip = None
dest_port = None

client_protocol = None
server_protocol = None

c2s_pending = []

c2s_data = ""
s2c_data = ""

g_msgs = []

IDX_TYPE = "type"
IDX_DATA = "data"
IDX_DIR = "direction"
IDX_TIME = "time"

g_frame = None

def parse_data(data, direction):
    while True:
        if len(data) < 4:
            break
        size = BitStream(bytes=data[:4]).uint
        if len(data) < size:
            break

        log.info("parse packet (size=%d)" % size)

        reader = BitReader(data[4:size])
        while True:
            type_and_data = type_descriptor.parse_game_msg(reader)
            if type_and_data is None:
                break
            msg = {
                IDX_TYPE : type_and_data[0],
                IDX_DATA : type_and_data[1],
                IDX_DIR : direction,
                IDX_TIME : time.strftime("%H:%M:%S"),
            }
            msg_index = len(g_msgs)
            g_msgs.append(msg)

            g_frame.add_row(msg_index)

        if reader.get_bit_len() > 7:
            log.warn("%d bits left" % reader.get_bit_len())

        data = data[size:]

    if len(data) > 0:
        log.warn("%d bytes left" % len(data))
    return data

class DestAddressProtocol(DatagramProtocol):

    def datagramReceived(self, data, (host, port)):
        log.info("received %s from %s:%d" % (data, host, port))

        global dest_ip, dest_port
        dest_ip, dest_port = data.split("|")
        dest_port = int(dest_port)

        endpoint = TCP4ServerEndpoint(reactor, 34889)
        endpoint.listen(ClientProtocolFactory())

class ClientProtocol(Protocol):

    def connectionMade(self):
        global client_protocol

        log.info("client connected")

        client_protocol = self

        reactor.connectTCP(dest_ip, dest_port, ServerProtocolFactory())

    def dataReceived(self, data):
        log.info("client send (len=%d)" % len(data))

        if server_protocol is None:
            log.info("postpone client send (len=%d)" % len(data))
            c2s_pending.append(data)
        else:
            global c2s_data
            c2s_data = parse_data(c2s_data + data, DIRECTION_C2S)
            server_protocol.transport.write(data)

class ClientProtocolFactory(Factory):

    protocol = ClientProtocol

class ServerProtocol(Protocol):

    def connectionMade(self):
        global server_protocol

        log.info("server connected")

        server_protocol = self

        while len(c2s_pending) > 0:
            data = c2s_pending.pop(0)
            log.info("resume client send (len=%d)" % len(data))

            global c2s_data
            c2s_data = parse_data(c2s_data + data, DIRECTION_C2S)
            self.transport.write(data)

    def dataReceived(self, data):
        log.info("server send (len=%d)" % len(data))

        global s2c_data
        s2c_data = parse_data(s2c_data + data, DIRECTION_S2C)
        client_protocol.transport.write(data)

class ServerProtocolFactory(ClientFactory):

    protocol = ServerProtocol

    def startedConnecting(self, connector):
        log.info("started to connect server")

    def clientConnectionLost(self, connector, reason):
        log.error("lost connection. Reason: " + str(reason))

    def clientConnectionFailed(self, connector, reason):
        log.error("connection failed. Reason:" + str(reason))

class MainFrame(wx.Frame):

    def __init__(self):
        wx.Frame.__init__(self, parent=None, title="Middle Man", size=(600, 400))

        splitter = wx.SplitterWindow(self)

        self.listctrl = wx.ListCtrl(splitter, -1, style=wx.LC_REPORT)
        self.listctrl.InsertColumn(0, "time", width=60)
        self.listctrl.InsertColumn(1, "name", width=200)
        self.listctrl.Bind(wx.EVT_LIST_ITEM_SELECTED, self.on_select)

        self.text = wx.TextCtrl(splitter, style=wx.TE_MULTILINE | wx.TE_READONLY)

        splitter.SplitVertically(self.listctrl, self.text)
        splitter.SetMinimumPaneSize(260)

        vsizer = wx.BoxSizer(wx.VERTICAL)
        vsizer.Add(splitter, 1, wx.EXPAND)
        self.SetSizer(vsizer)

        default_bitmap = wx.EmptyBitmap(1, 1)

        toolbar = self.CreateToolBar(wx.TB_HORZ_TEXT|wx.TB_NOICONS)
        toolbar.AddLabelTool(1, 'Filter Type', default_bitmap)
        toolbar.AddLabelTool(2, 'Filter Data', default_bitmap)
        toolbar.AddLabelTool(3, 'Show All', default_bitmap)
        toolbar.Realize()

        self.Bind(wx.EVT_TOOL, self.filter_type, id=1)
        self.Bind(wx.EVT_TOOL, self.filter_data, id=2)
        self.Bind(wx.EVT_TOOL, self.show_all, id=3)

        self.statusBar = self.CreateStatusBar()

        self.type_filter = None

    def on_select(self, event):
        index = event.GetIndex()
        msg_index = self.listctrl.GetItemData(index)
        children = g_msgs[msg_index]

        self.text.SetValue(pprint.pformat(children[IDX_DATA]))

    def add_row(self, msg_index):
        msg = g_msgs[msg_index]

        if self.type_filter is not None and msg[IDX_TYPE] not in self.type_filter:
            return

        index = self.listctrl.InsertStringItem(sys.maxint, msg[IDX_TIME])
        self.listctrl.SetStringItem(index, 1, msg[IDX_TYPE])
        self.listctrl.SetItemData(index, msg_index)
        if msg[IDX_DIR] == DIRECTION_C2S:
            self.listctrl.SetItemBackgroundColour(index, "grey")

        self.statusBar.SetStatusText("%d / %d" % (self.listctrl.GetItemCount(), len(g_msgs)))

    def refresh_rows(self):
        self.listctrl.DeleteAllItems()
        for i in xrange(len(g_msgs)):
            self.add_row(i)

    def filter_type(self, event):
        msg_types = set()
        index = self.listctrl.GetFirstSelected()
        while index != -1:
            msg_index = self.listctrl.GetItemData(index)
            msg = g_msgs[msg_index]
            msg_types.add(msg[IDX_TYPE])
            index = self.listctrl.GetNextSelected(index)

        if len(msg_types) == 0:
            wx.MessageBox("No messages selected.", "D'oh!", wx.OK | wx.ICON_ERROR)
            return

        self.type_filter = msg_types

        self.refresh_rows()

    def filter_data(self, event):
        wx.MessageBox("Not Implemented yet.", "Ay Carumba!", wx.OK | wx.ICON_INFORMATION)

    def show_all(self, event):
        self.type_filter = None
        self.refresh_rows()

def main():
    log.set_log_file("message.log")
    type_descriptor.load_xml("attributes.xml", "typedescriptors.xml")

    app = wx.App(False) 

    global g_frame
    g_frame = MainFrame() 
    g_frame.Show() 
    reactor.registerWxApp(app)

    reactor.listenUDP(34888, DestAddressProtocol())
    reactor.run()

if __name__ == '__main__':
    main()
