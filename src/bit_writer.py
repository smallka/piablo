from bitstring import BitStream

class BitWriter:
    def __init__(self):
        self._bytes = ""
        self._bits = BitStream()

    def write_int(self, length, value):
        news = BitStream(uint=value, length=length)
        start = 0

        if self._bits.len > 0:
            left = 8 - self._bits.len
            if news.len < left:
                left = news.len
            self._bits = news[:left] + self._bits
            start += left

            if self._bits.len == 8:
                self._bytes += self._bits.tobytes()
                self._bits = BitStream()

        byte_len = (news.len - start) / 8
        if byte_len > 0:
            more = byte_len * 8
            self._bytes += news[start:start+more].tobytes()
            start += more

        if news.len > start:
            self._bits = news[start:]

    def write_int64(self, length, value):
        if length <= 32:
            self.write_int(length, value)
            return
        count = length - 32
        self.write_int(32, value >> count)
        self.write_int(count, value & ((1 << count) - 1))

    def get_bytes(self):
        if self._bits.len > 0:
            more = 8 - self._bits.len
            tail = (BitStream(int=0, length=more) + self._bits).tobytes()
            return self._bytes + tail
        else:
            return self._bytes
