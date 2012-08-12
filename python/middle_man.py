from twisted.internet.protocol import DatagramProtocol
from twisted.internet.protocol import Protocol
from twisted.internet.protocol import ClientFactory
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

from bitstring import BitStream
from bit_reader import BitReader

dest_ip = None
dest_port = None

client_protocol = None
server_protocol = None

c2s_queue = []

def EntityId(reader):
	print "EntityId", reader.read_int64(64), reader.read_int64(64)

def GameId(reader):
	print "GameId", reader.read_int64(64), reader.read_int64(64), reader.read_int64(64)

def JoinBNetGameMessage(reader):
	EntityId(reader)
	GameId(reader)
	print reader.read_int(32)
	print reader.read_int64(64)
	print reader.read_int(4)
	print "ProtoHash", reader.read_int(32)
	print "SnoPackHash", reader.read_int(32)


g_opcodes = {
	10 : JoinBNetGameMessage, 
}

def parseData(data):
	while True:
		if len(data) < 4:
			break
		size = BitStream(bytes=data[:4]).uint
		if len(data) < size:
			print "ERROR: incomplete packet"

		reader = BitReader(data[4:size])
		while reader.get_bit_len() >= 9:
			opcode = reader.read_int(9)
			if opcode not in g_opcodes:
				print "unrecognized opcode", opcode
				break
			else:
				func = g_opcodes[opcode]
				print "[%d] %s" % (opcode, func.__name__)
				func(reader)

		data = data[size:]

	if len(data) > 0:
		print "ERROR: %d bytes unrecognized", len(data)

class DestAddressProtocol(DatagramProtocol):

	def datagramReceived(self, data, (host, port)):
		print "received %s from %s:%d" % (data, host, port)

		global dest_ip, dest_port
		dest_ip, dest_port = data.split("|")
		dest_port = int(dest_port)

		endpoint = TCP4ServerEndpoint(reactor, 34889)
		endpoint.listen(ClientProtocolFactory())

class ClientProtocol(Protocol):

	def connectionMade(self):
		global client_protocol

		print "client connected"

		client_protocol = self

		reactor.connectTCP(dest_ip, dest_port, ServerProtocolFactory())

	def dataReceived(self, data):
		print "client send (len=%d)" % len(data)

		if server_protocol is None:
			print "postpone client send (len=%d)" % len(data)
			c2s_queue.append(data)
		else:
			parseData(data)
			server_protocol.transport.write(data)

class ClientProtocolFactory(Factory):

	protocol = ClientProtocol

class ServerProtocol(Protocol):

	def connectionMade(self):
		global server_protocol

		print "server connected"

		server_protocol = self

		while len(c2s_queue) > 0:
			data = c2s_queue.pop(0)
			print "resume client send (len=%d)" % len(data)
			parseData(data)
			self.transport.write(data)

	def dataReceived(self, data):
		print "server send (len=%d)" % len(data)

		parseData(data)
		client_protocol.transport.write(data)

class ServerProtocolFactory(ClientFactory):

	protocol = ServerProtocol

	def startedConnecting(self, connector):
		print "started to connect server"

	def clientConnectionLost(self, connector, reason):
		print "lost connection. Reason:", reason

	def clientConnectionFailed(self, connector, reason):
		print "connection failed. Reason:", reason

def main():
	reactor.listenUDP(34888, DestAddressProtocol())
	reactor.run()

if __name__ == '__main__':
	main()
