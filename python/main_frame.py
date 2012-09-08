import sys
import wx
import pprint

from define import *

class MainFrame(wx.Frame):

    def __init__(self):
        wx.Frame.__init__(self, parent=None, title="Middle Man", size=(600, 400))

        self.conn = None

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

        self.statusBar = self.CreateStatusBar()
        self.statusBar.SetStatusText("waiting for connection")

        self.type_filter = None
        self.data_filter = None

        self.type_hidden = set([
            "GameTickMessage",
            "DWordDataMessage10",
            "DWordDataMessage11",
            "SimpleMessage16",
        ])

        # TODO: tmp
        self.setup_connection(None)

    def setup_connection(self, conn):
        self.conn = conn

        default_bitmap = wx.EmptyBitmap(1, 1)

        toolbar = self.CreateToolBar(wx.TB_HORZ_TEXT|wx.TB_NOICONS)
        toolbar.AddLabelTool(1, 'Filter Type', default_bitmap)
        toolbar.AddLabelTool(2, 'Filter Data', default_bitmap)
        toolbar.AddLabelTool(3, 'Clear Filter', default_bitmap)
        toolbar.AddLabelTool(4, 'Hide Type', default_bitmap)
        toolbar.AddLabelTool(5, 'Clear All', default_bitmap)
        toolbar.Realize()

        self.Bind(wx.EVT_TOOL, self.filter_type, id=1)
        self.Bind(wx.EVT_TOOL, self.filter_data, id=2)
        self.Bind(wx.EVT_TOOL, self.clear_filter, id=3)
        self.Bind(wx.EVT_TOOL, self.hide_type, id=4)
        self.Bind(wx.EVT_TOOL, self.clear_all, id=5)

    def on_select(self, event):
        index = event.GetIndex()
        msg_index = self.listctrl.GetItemData(index)
        msg = self.conn.get_msg(msg_index)

        self.text.SetValue(pprint.pformat(msg[IDX_MSG_DATA]))

    def add_row(self, msg_index):
        msg = self.conn.get_msg(msg_index)

        if msg[IDX_MSG_TYPE] in self.type_hidden:
            return

        if self.type_filter is not None \
                and msg[IDX_MSG_TYPE] not in self.type_filter:
            return

        if self.data_filter is not None \
                and not type_descriptor.text_in_msg(msg[IDX_MSG_DATA], self.data_filter):
            return

        index = self.listctrl.InsertStringItem(sys.maxint, msg[IDX_MSG_TIME])
        self.listctrl.SetStringItem(index, 1, msg[IDX_MSG_TYPE])
        self.listctrl.SetItemData(index, msg_index)
        if msg[IDX_MSG_DIR] == DIRECTION_C2S:
            self.listctrl.SetItemBackgroundColour(index, "grey")

    def refresh_statusbar(self):
        text = "%d / %d" % (self.listctrl.GetItemCount(), self.conn.get_msg_count())
        self.statusBar.SetStatusText(text)

    def refresh_rows(self):
        self.listctrl.DeleteAllItems()
        for i in xrange(self.conn.get_msg_count()):
            self.add_row(i)

        self.refresh_statusbar()

    def filter_type(self, event):
        msg_types = set()
        index = self.listctrl.GetFirstSelected()
        while index != -1:
            msg_index = self.listctrl.GetItemData(index)
            msg = self.conn.get_msg(msg_index)
            msg_types.add(msg[IDX_MSG_TYPE])
            index = self.listctrl.GetNextSelected(index)

        if len(msg_types) == 0:
            wx.MessageBox("No messages selected.", "D'oh!", wx.OK | wx.ICON_ERROR)
            return

        self.type_filter = msg_types

        self.refresh_rows()

    def filter_data(self, event):
        text = wx.GetTextFromUser("Enter the text", "Filter Data Dialog")
        if text.strip() == "":
            wx.MessageBox("No data input.", "D'oh!", wx.OK | wx.ICON_ERROR)
            return

        self.data_filter = text

        self.refresh_rows()

    def clear_filter(self, event):
        self.type_filter = None
        self.data_filter = None
        self.refresh_rows()

    def hide_type(self, event):
        type_mask_dialog = TypeMaskDialog(None, title="Type Mask Dialog")
        ret = type_mask_dialog.ShowModal()
        print type_mask_dialog.show_list.GetItems()
        print type_mask_dialog.hide_list.GetItems()
        type_mask_dialog.Destroy()

        """
        msg_types = set()
        index = self.listctrl.GetFirstSelected()
        while index != -1:
            msg_index = self.listctrl.GetItemData(index)
            msg = self.conn.get_msg(msg_index)
            msg_types.add(msg[IDX_MSG_TYPE])
            index = self.listctrl.GetNextSelected(index)

        if len(msg_types) == 0:
            wx.MessageBox("No messages selected.", "D'oh!", wx.OK | wx.ICON_ERROR)
            return

        self.type_hidden = self.type_hidden.union(msg_types)

        self.refresh_rows()
        """

    def clear_all(self, event):
        self.conn.remove_all_msg()
        self.refresh_rows()


class TypeMaskDialog(wx.Dialog):

    def __init__(self, *args, **kw):
        super(TypeMaskDialog, self).__init__(*args, **kw)

        left_panel = wx.Panel(self)
        left_vbox = wx.BoxSizer(wx.VERTICAL)
        left_list = wx.ListBox(left_panel, -1, style=wx.LB_SORT)
        left_list.Append("Hello")
        self.Bind(wx.EVT_LISTBOX_DCLICK, self.OnHide, left_list)

        left_vbox.Add(wx.StaticText(left_panel, label="Show"), 0, wx.EXPAND)
        left_vbox.Add(left_list, 1, wx.EXPAND)
        left_panel.SetSizer(left_vbox)

        right_panel = wx.Panel(self)
        right_vbox = wx.BoxSizer(wx.VERTICAL)
        right_list = wx.ListBox(right_panel, -1, style=wx.LB_SORT)
        right_list.Append("world")
        self.Bind(wx.EVT_LISTBOX_DCLICK, self.OnShow, right_list)

        right_vbox.Add(wx.StaticText(right_panel, label="Hide"), 0, wx.EXPAND)
        right_vbox.Add(right_list, 1, wx.EXPAND)
        right_panel.SetSizer(right_vbox)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(left_panel, 1, wx.EXPAND)
        hbox.Add(right_panel, 1, wx.EXPAND)
        self.SetSizer(hbox)

        self.show_list = left_list
        self.hide_list = right_list

    def OnHide(self, event):
        sel = self.show_list.GetSelection()
        value = self.show_list.GetString(sel)

        self.show_list.Delete(sel)
        self.hide_list.Append(value)

    def OnShow(self, event):
        sel = self.hide_list.GetSelection()
        value = self.hide_list.GetString(sel)

        self.hide_list.Delete(sel)
        self.show_list.Append(value)
