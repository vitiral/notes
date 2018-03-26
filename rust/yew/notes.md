
# Components
These are actually really cool.
- You implement the following:
  - `Component` (provides `update() + create()`) and has a _specific_ `Msg` type.
  - Renderable` (provides `view()`) traits.
- The `view()` method can send out _any_ type of message (_what!!!_) and they
  will go to _every_ `update()` method that takes that type.
  - Hmm... it looks as if this is not accurate. It can only send messages
    defined in it's `Compnent::Msg`. It can _also_ use the defined `Callback<T>`
    to send messages, which has the `emit` method. This is all handled semi-automagically
    by the `html` macro (I guess).
- In the same way, the `update()` can receive messages through its `view()` method.
