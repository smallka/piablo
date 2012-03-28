import random

from bit_reader import BitReader
from bit_writer import BitWriter

WORDS = "AAAAB3NzaC1yc2EAAAADAQABAAABAQDBXfPQYSwAtDSg6rkGhxqsAXXdGoaQAxEWl16j8j97eFtmKecBms5sO87SPBNfZIJeICPjC0jU7jvEkUVBq0SL2kBE60uwYRdUD51oYtT8JjNzelMxWOMFtQtp+ZGVmh1xYRE2S4bacYC5mQ9YYTUVUDOEeBx88MuzhZWJs1bh4WRHHV9N1e09CTDI1xx8XE+RfXST0ZMzGLPXUnzKkGWvRuSUJAHVNdUwXC32TgVCTza2sSj1Sxxol4RkrSh73Kgmt/hIOqEx8EHEyQIS+XckepBTPBKOJVscXzFixdo8jnvIqPC0Th11F/CfoWIExcYu5UqP/ckXmKjObeUkWVhP"

def test_char_array():
    writer = BitWriter()
    for i in xrange(len(WORDS)):
        writer.write_char_array(len(WORDS) + i, WORDS)
    reader = BitReader(writer.get_bytes())
    for i in xrange(len(WORDS)):
        assert WORDS == reader.read_char_array(len(WORDS) + i)

    for i in xrange(1000):
        start = random.randint(0, len(WORDS)-1)
        end = random.randint(start+1, len(WORDS))
        sample = WORDS[start:end]
        max_length = random.randint(len(sample), len(sample) * 2)

        writer = BitWriter()
        writer.write_char_array(max_length, sample)
        reader = BitReader(writer.get_bytes())
        assert sample == reader.read_char_array(max_length)

if __name__ == "__main__":
    test_char_array()
