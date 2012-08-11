from twisted.internet.protocol import DatagramProtocol
from twisted.internet.protocol import Protocol
from twisted.internet.protocol import ClientFactory
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

dest_ip = None
dest_port = None

client_protocol = None
server_protocol = None

c2s_queue = []

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
			self.transport.write(data)

	def dataReceived(self, data):
		print "server send (len=%d)" % len(data)

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
