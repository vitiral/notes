# Compression algorithm
# http://www.cse.yorku.ca/~oz/hash.html
# https://marc-b-reynolds.github.io/math/2017/10/13/IntegerBijections.html
# https://stackoverflow.com/questions/4273466/reversible-hash-function


def u32(v:int): return 0xFFFFFFFF & v
def tochk(v:int): return bytearray(v.to_bytes(4, byteorder='big'))

def sdbm(seed, barr):
    for b in barr:
        seed = b + u32(seed << 6) + u32(seed << 16) - seed
    return seed


# class Uncompressible(Exception):
#     pass
# 
# def compress(seed: int, chk: int, data: bytearray):
#     """ Compress data.
# 
#     Seed is a 32 bit integer, chk 32bit with the bottom 8
#     bits for the flag.
#     """
#     seed = u32(seed)
#     chk = tochk(chk)
#     i = 0
#     result = bytearray()
#     for byte in data:
#         
#     return result
# 
