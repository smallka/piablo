from bitstring import BitStream
from util import bit_count

class BitReader:
    def __init__(self, data):
        self._bytes = data
        self._bits = BitStream()

    def read_int(self, length):
        result = 0

        if self._bits.len > 0:
            if length < self._bits.len:
                left = self._bits.len - length
                result = self._bits[left:].uint
                self._bits = self._bits[:left]
                return result
            length -= self._bits.len
            result |= self._bits.uint << length
            self._bits = BitStream()

        byte_len = length / 8
        if byte_len > 0:
            length -= byte_len * 8
            bits = BitStream(bytes=self._bytes[:byte_len])
            result |= bits.uint << length
            self._bytes = self._bytes[byte_len:]

        if length > 0:
            one_byte = BitStream(bytes=self._bytes[0])
            left = 8 - length
            result |= one_byte[left:].uint
            self._bits = one_byte[:left]
            self._bytes = self._bytes[1:]

        return result

    def read_int64(self, length):
        if length <= 32:
            return self.read_int(self, length)

        count = length - 32
        return (self.read_int(32) << count) | self.read_int(count)

    def read_float32(self):
        value = BitStream("uint:32=%d" % self.read_int(32))
        return BitStream(value).float

    def read_char_array(self, max_length):
        size = self.read_int(bit_count(max_length))

        self._bits = BitStream()
        result = self._bytes[:size]
        self._bytes = self._bytes[size:]
        return result

    def get_bit_len(self):
        return len(self._bytes) * 8 + self._bits.len
