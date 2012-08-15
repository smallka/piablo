from twisted.internet.protocol import DatagramProtocol
from twisted.internet.protocol import Protocol
from twisted.internet.protocol import ClientFactory
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

from bitstring import BitStream
from bit_reader import BitReader
import type_descriptor

dest_ip = None
dest_port = None

client_protocol = None
server_protocol = None

c2s_queue = []

def parse_data(data):
	while True:
		if len(data) < 4:
			break
		size = BitStream(bytes=data[:4]).uint
		if len(data) < size:
			print "ERROR: incomplete packet"

		reader = BitReader(data[4:size])
		while type_descriptor.parse_game_msg(reader):
			pass

		if reader.get_bit_len() > 0:
			print "WARNING: %d bits left" % reader.get_bit_len()

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
			parse_data(data)
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
			parse_data(data)
			self.transport.write(data)

	def dataReceived(self, data):
		print "server send (len=%d)" % len(data)

		parse_data(data)
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
	type_descriptor.load_xml("C:\\download\\typedescriptors.xml")

	reactor.listenUDP(34888, DestAddressProtocol())
	reactor.run()

if __name__ == '__main__':
	main()
