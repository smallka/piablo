from bit_writer import BitWriter
from bitstring import BitStream

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

import socket
import sys
 
HOST = "127.0.0.1"
PORT = 1999

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

    while True:
        data = BitStream(bytes=sock.recv(1024))
        print data
        if data.len <= 0:
            break
    sock.close()
                      
if __name__ == "__main__":
    start()
