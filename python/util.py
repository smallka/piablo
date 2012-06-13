def bit_count(value):
    count = 0
    while value > 0:
        value >>= 1
        count += 1
    return count

