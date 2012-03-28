import socket
import sys

from bitstring import BitStream
from bit_reader import BitReader
from bit_writer import BitWriter
 
HOST = "127.0.0.1"
PORT = 1999

toon_entity_id = (0, 0x00000000E23C2077)
field1_game_id = (
    0xC5BEEC600D8C627B,
    0x0600000000000000,
    0x0000000000000007)
game_id = 7
field3 = 0xC0B2F7EF5325B824 
field4 = 2
proto_hash = 0x375AE194
sno_pack_hash = 0x16765C7B

def write_entity_id(writer, entity_id):
    writer.write_int64(64, entity_id[0])
    writer.write_int64(64, entity_id[1])
    
def write_game_id(writer, game_id):
    writer.write_int64(64, game_id[0])
    writer.write_int64(64, game_id[1])
    writer.write_int64(64, game_id[2])

def join_bnet_game_message(writer):   
    write_entity_id(writer, toon_entity_id)
    write_game_id(writer, field1_game_id)
    writer.write_int(32, game_id)
    writer.write_int64(64, field3)
    writer.write_int(4, field4 - 2)
    writer.write_int(32, proto_hash)
    writer.write_int(32, sno_pack_hash)

def send_msg(sock, msg_id, msg_func):
    writer = BitWriter()
    writer.write_int(9, msg_id)
    msg_func(writer)
    msg = writer.get_bytes()
    header = BitStream(uint=len(msg) + 4, length=32).tobytes()
    sock.send(header + msg)

def try_get_packet_size(data):
    if len(data) < 4:
        return None
    size = BitStream(bytes=data[:4]).uint
    if len(data) < size:
        return None
    return size

def version_message(reader):
    print "SNOPackHash: 0x%x" % reader.read_int(32)
    print "ProtocolHash: 0x%x" % reader.read_int(32)
    print "Version: %s" % reader.read_char_array(32)

def connection_established_message(reader):
    print "PlayerIndex: 0x%x" % reader.read_int(3)
    print "Field1: 0x%x" % reader.read_int(32)
    print "SNOPackHash: 0x%x" % reader.read_int(32)

def game_setup_message(reader):
    print "Field0: 0x%x" % reader.read_int(32)

def save_point_info_message(reader):
    print "snoLevelArea: 0x%x" % reader.read_int(32)
    
MESSAGE_DISPATCHER = {
    13 : version_message,
    49 : connection_established_message,
    50 : game_setup_message,
    73 : save_point_info_message,
}

def start():
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    except socket.error, msg:
        sys.stderr.write("[ERROR] %s\n" % msg[1])
        sys.exit(1)
         
    try:
        sock.connect((HOST, PORT))
    except socket.error, msg:
        sys.stderr.write("[ERROR] %s\n" % msg[1])
        sys.exit(2)
            
    send_msg(sock, 10, join_bnet_game_message)

    incoming = ""
    while True:
        data = sock.recv(1024)
        if len(data) == 0:
            break
        incoming += data
        while True:
            size = try_get_packet_size(incoming)
            if size is None:
                break
            reader = BitReader(incoming[4:size])
            while reader.get_bit_len() >= 9:
                opcode = reader.read_int(9)
                print "------ opcode %d --------" % opcode
                if opcode not in MESSAGE_DISPATCHER:
                    sock.close()
                    return
                MESSAGE_DISPATCHER[opcode](reader)

            incoming = incoming[size:]

    sock.close()
                      
if __name__ == "__main__":
    start()
