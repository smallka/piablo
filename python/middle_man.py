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

import log
import type_descriptor
from main_frame import MainFrame
from connection import Connection

class DestAddressProtocol(DatagramProtocol):

    def __init__(self, frame):
        self.frame = frame

    def datagramReceived(self, data, (host, port)):
        #log.info("received %s from %s:%d" % (data, host, port))
        server_ip, server_port = data.split("|")
        server_port = int(server_port)

        endpoint = TCP4ServerEndpoint(reactor, 34889)
        endpoint.listen(ClientProtocolFactory(self.frame, server_ip, server_port))

class ClientProtocol(Protocol):

    def __init__(self, frame, server_ip, server_port):
        self.server_ip = server_ip
        self.server_port = server_port
        self.conn = Connection(frame)
        frame.setup_connection(self.conn)

    def connectionMade(self):
        log.info("client connected")

        self.conn.setup_client(self)

        reactor.connectTCP(self.server_ip, self.server_port, ServerProtocolFactory(self.conn))

    def dataReceived(self, data):
        #log.info("client send (len=%d)" % len(data))
        self.conn.receive_client(data)

class ClientProtocolFactory(Factory):

    def __init__(self, frame, server_ip, server_port):
        self.frame = frame
        self.server_ip = server_ip
        self.server_port = server_port

    def buildProtocol(self, addr):
        return ClientProtocol(self.frame, self.server_ip, self.server_port)

class ServerProtocol(Protocol):

    def __init__(self, conn):
        self.conn = conn

    def connectionMade(self):
        log.info("server connected")
        self.conn.setup_server(self)
        
    def dataReceived(self, data):
        #log.info("server send (len=%d)" % len(data))
        self.conn.receive_server(data)

class ServerProtocolFactory(ClientFactory):

    def __init__(self, conn):
        self.conn = conn

    def buildProtocol(self, addr):
        return ServerProtocol(self.conn)

    def startedConnecting(self, connector):
        log.info("started to connect server")

    def clientConnectionLost(self, connector, reason):
        log.error("lost connection. Reason: " + str(reason))

    def clientConnectionFailed(self, connector, reason):
        log.error("connection failed. Reason:" + str(reason))


def main():
    log.set_log_file("message.log")
    type_descriptor.load_xml("attributes.xml", "typedescriptors.xml")

    app = wx.App(False) 

    # NOTE: frame must exist before run
    frame = MainFrame()
    frame.Show()

    reactor.registerWxApp(app)

    reactor.listenUDP(34888, DestAddressProtocol(frame))
    reactor.run()

if __name__ == '__main__':
    main()
