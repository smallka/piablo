from bitstring import BitStream

class BitWriter:
    def __init__(self):
        self._bytes = ""
        self._bits = BitStream()

    def write_int(self, length, value):
        bits = BitStream(int=value, length=length)
        start = 0

        if self._bits.len > 0:
            left = 8 - self._bits.len
            more = bits.len > left and left or bits.len
            self._bits = bits[:more] + self._bits
            start += more

            if self._bits.len == 8:
                self._bytes += self._bits.tobytes()
                self._bits = BitStream()

        byte_len = (bits.len - start) / 8
        if byte_len > 0:
            more = byte_len * 8
            self._bytes += bits[start:start+more].tobytes()
            start += more

        if bits.len > start:
            self._bits = bits[start:]

    def get_bytes(self):
        if self._bits.len > 0:
            more = 8 - self._bits.len
            tail = (BitStream(int=0, length=more) + self._bits).tobytes()
            return self._bytes + tail
        else:
            return self._bytes
