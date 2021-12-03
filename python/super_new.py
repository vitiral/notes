from inspect import signature

class JustInit(object):
    def __init__(self, a, b):
        self.instance_a = a
        self.instance_b = b


class PrintNew(object):
  def __new__(cls, *args, **kwargs):
    print("new args:", args, "new kwargs:", kwargs)
    return super().__new__(cls)

  def __init__(self, a):
    self.a = a

print("PrintNew")
print(signature(PrintNew.__new__))
print(signature(PrintNew.__init__))

pnew = PrintNew(42)
print(pnew.a)


> ```
> PrintNew
> (cls, *args, **kwargs)
> (self, a)
> new args: (42,) new kwargs: {}
> 42
> ```
