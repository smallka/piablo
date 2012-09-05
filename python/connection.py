import time
from bitstring import BitStream

from define import *
import log
import type_descriptor
from bit_reader import BitReader
from main_frame import MainFrame

class Connection:

    def __init__(self, frame):
        self.server_socket = None
        self.client_socket = None
        self.data_to_server = ""
        self.data_to_client = ""
        self.pending_to_server = []
        self.msgs = []
        self.frame = frame

    def setup_client(self, client_socket):
        self.client_socket = client_socket

    def setup_server(self, server_socket):
        self.server_socket = server_socket

        while len(self.pending_to_server) > 0:
            data = self.pending_to_server.pop(0)
            log.info("resume data from client (len=%d)" % len(data))

            self.parse_data(data, DIRECTION_C2S)
            self.server_socket.transport.write(data)

    def receive_client(self, data):
        if self.server_socket is None:
            log.info("postpone data from client (len=%d)" % len(data))
            self.pending_to_server.append(data)
        else:
            self.parse_data(data, DIRECTION_C2S)
            self.server_socket.transport.write(data)

    def receive_server(self, data):
        self.parse_data(data, DIRECTION_S2C)
        self.client_socket.transport.write(data)        

    def get_msg(self, msg_index):
        return self.msgs[msg_index]

    def get_msg_count(self):
        return len(self.msgs)

    def remove_all_msg(self):
        self.msgs = []

    def parse_data(self, data, direction):
        if direction == DIRECTION_C2S:
            data = self.data_to_server + data
        else:
            data = self.data_to_client + data

        time_str = time.strftime("%H:%M:%S")
        while True:
            if len(data) < 4:
                break
            size = BitStream(bytes=data[:4]).uint
            if len(data) < size:
                break

            #log.info("parse packet (size=%d)" % size)

            reader = BitReader(data[4:size])
            while True:
                type_and_data = type_descriptor.parse_game_msg(reader)
                if type_and_data is None:
                    break
                msg = {
                    IDX_MSG_TYPE : type_and_data[0],
                    IDX_MSG_DATA : type_and_data[1],
                    IDX_MSG_DIR : direction,
                    IDX_MSG_TIME : time_str,
                }
                msg_index = len(self.msgs)
                self.msgs.append(msg)

                self.frame.add_row(msg_index)

            if reader.get_bit_len() > 7:
                log.warn("%d bits left" % reader.get_bit_len())

            data = data[size:]

        if len(data) > 0:
            log.warn("%d bytes left" % len(data))

        if direction == DIRECTION_C2S:
            self.data_to_server = data
        else:
            self.data_to_client = data

        self.frame.refresh_statusbar()

