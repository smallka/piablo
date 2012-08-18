from twisted.internet.protocol import DatagramProtocol
from twisted.internet.protocol import Protocol
from twisted.internet.protocol import ClientFactory
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

from bitstring import BitStream
from bit_reader import BitReader
import log
import type_descriptor

dest_ip = None
dest_port = None

client_protocol = None
server_protocol = None

c2s_pending = []

c2s_data = ""
s2c_data = ""

def parse_data(data):
	while True:
		if len(data) < 4:
			break
		size = BitStream(bytes=data[:4]).uint
		if len(data) < size:
			break

		log.info("parse packet (size=%d)" % size)

		reader = BitReader(data[4:size])
		while type_descriptor.parse_game_msg(reader):
			pass

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
			c2s_data = parse_data(c2s_data + data)
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
			c2s_data = parse_data(c2s_data + data)
			self.transport.write(data)

	def dataReceived(self, data):
		log.info("server send (len=%d)" % len(data))

		global s2c_data
		s2c_data = parse_data(s2c_data + data)
		client_protocol.transport.write(data)

class ServerProtocolFactory(ClientFactory):

	protocol = ServerProtocol

	def startedConnecting(self, connector):
		log.info("started to connect server")

	def clientConnectionLost(self, connector, reason):
		log.error("lost connection. Reason: " + str(reason))

	def clientConnectionFailed(self, connector, reason):
		log.error("connection failed. Reason:" + str(reason))

def main():
	log.set_log_file("message.log")
	type_descriptor.load_xml(
		"C:\\download\\attributes.xml",
		"C:\\download\\typedescriptors.xml")

	reactor.listenUDP(34888, DestAddressProtocol())
	reactor.run()

if __name__ == '__main__':
	main()
