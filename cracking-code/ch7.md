# PG 58 CH7: Object Oriented Design

## 7.1
> Design the data structures for a generic deck of cards
> Explain how you would sub class it to implement particular card games.


There would obviously be the base classes:

```
class Deck(object):
    """
    A deck of cards.

    treated as a FIFO buffer.
    """

    def __init__(self, cards: List[Card]):
        self.cards = cards

    def shuffle(self):
        """Shuffle the deck randomly."""
        pass

    def draw(self, num=1):
        """Draw a the number of cards from the top of the deck.

        The cards are removed from the deck.
        """
        pass

    def random(self, num=1):
        """Get a random set of cards.

        The cards are removed from the deck.
        """

    def insert(self, cards: List[Card], index=0):
        """Insert a list of cards into the deck."""

    def extend(self, cards: List[Card]):
        """Add cards to the top of the deck.
        """


class Card(object):
    def __init__(self, suite: CardSuite, value: CardValue):
        self.suite = suite
        self.value = value

    def __eq__(self, other: Card):
        return self.suite == other.suite and self.value == other.value

    def __gt__(self, other: Card):
        """... all comparison operators."""


class CardSuite(enum.Enum):
    heart = "heart"
    diamond = "diamond"
    club = "club"
    spade = "spade"

    def __gt__(self, other: Suite):
        """... all comparison operators."""


class CardValue(enum.Enum):
    one = 1
    two = 2
    ...
    nine = 9
    ten = 10
    jack = 11
    queen = 12
    king = 13
    joker = 100

    def __gt__(self, other: CardValue):
        """... all comparison operators."""
```

Particular card games could subclass `Deck` or even `Card` to help them implement
their logic.

```
class PokerDeck(Deck):
    def deal_poker_hand(self) -> PokerHand:
        """Remove 5 cards from the deck, putting them in the poker hand."""

    def deal_one(self, hands: List[PokerHand]):
        """Insert a card into each hand, being sure to throw away the first card."""


class PokerHand(object):
    def __init__(self, cards: Set[Card]):
        self.cards = cards

        # suites+values make it easy to determine
        # 4-of-a-kind, flush, etc.
        self.suites = {}
        for c in cards:
            if c.suite not in self.suites:
                self.suites[c] = 0
            self.suites[c] += 1

        self.values = {}
        # ... same as suites

    def insert(self, card: Card):
        """Insert a card into the hand."""

    def max_4ofakind(self) -> Option[Card]:
        """If this is "4 of a kind" return the highest card.

        Else return None.
        """

    def max_2ofakind(self) -> Option[Card]:
        """etc..."""

    def __gt__(self, other: PokerHand):
        """... all comparison operators."""
```


# 7.2
> Imagine you have a call center with three levels of employees: fresher,
> technical lead (TL), product manager (PM)
>
> There can be multiple employees, but only one TL or PM. An incoming telephone
> call must be allocated to a fresher who is free. If a fresher can’t
> handle the call, he or she must escalate the call to technical lead
>
> If the TL is not free or not able to handle it, then the call should be
> escalated to PM. Design the classes and data structures for this problem
> Implement a method getCallHandler()

```

class CallCenter(object):
    def __init__(self, freshers: List[Fresher], tech_lead, prod_man):
        self.freshers = freshers
        self.tech_lead = tech_lead
        self.prod_man = prod_man

    def getCallHandler(self) -> Employee:
        if self.get_fresher_handler():
            return
        if self.tech_lead.attempt_call():
            return
        if self.prod_man.attemp_call():
            return
        raise CallNotHandled()

    def get_fresher_handle(self):
        for fresher in self.freshers:
            if fresher.attempt_call():
                return True

class Employee(object):
    def __init__(self):
        self.lock = threading.Lock()

    def attempt_call(self):
        """Attempt to handle the call.

        If the call is being handled return True, else False.
        """
        got_lock = self.lock.acquire(block=False)
        if not got_lock:
            return False  # call is not being handled
        threading.spawn(target=self._handle_call)
        return True

    def _handle_call(self):
        handle_call()
        self.lock.release()


```

## Conclusion
I got way too into the weed here. I should have focused on the objects themselves
and how to design them. I tried to come up with a multihreaded solution without
really thinking about:
- What does the `Call` object look like?
- Should each kind of employee have an object? What do they look like?
- etc...

Clearly a conversation would have _helped_ me, but it is good to know the general direction
that is inteded to go.


# 7.3
> Design a musical juke box using object oriented principles

It is a musical juke box, what does it do?
- Accepts payment
- Plays music
- Handles a bunch of _different CD's_, each of which have different tracks.
- Accepts customer's choice in music which gets appended to the queue.
  - The customer can choose a sequence _any tracks_ (regardless of CD's)
  - Any customer can always "add next track" to the queue.


```

class Jukebox(object):
    def __init__(self, cds: List[Cd], song_cost: Cost):
        self.cds = {cd.name: cd for cd in cds}
        self.song_cost = song_cost

        # TODO: make multithreaded
        self.play_songs = queue()

    def append_song(self, payment, cd_name, song_name):
        """Accepts payment for a single song.
        """
        cd = self.cds.get(cd_name)
        if not cd:
            raise CdNotFoundError(cd)
        song = cd.get(song_name)
        if not song:
            raise SongNotFoundError(cd_name, song_name)
        self.play_songs.append((cd, song))

    def play_loop(self):
        """This method should be run in a separate thread.

        It makes sure that songs continue to play as long as they
        are in the queue.
        """
        while True:
            if play_songs:
                cd, song = play_songs.pop()
                self.display_playing(cd, song)
                song.play()
            else:
                self.display_playing(None, None)

            time.sleep(1)  # or block until item enters queue

    def accept_new_songs(self):
        """This method should be run in a spearate thread.

        It waits for the users's song input. When a song
        selection has been made it accepts payment
        and then plays the song.
        """

        # TODOs:
        # - only accept up to N number of songs
        # - cache payment in some way instead of refunding
        while True:
            cd, song = self.get_new_song()
            payment = self.get_payment()
            if payment.amount != self.song_cost != 0:
                payment.refund()
                self.message_to_user(
                    "The cost of a song must be exactly {}".format(self.song_cost)
                )
                continue
            try:
                self.append_song(cd, song)
            except SongNotPlayedError as e:
                payment.refund()
                self.message_to_user(e)

    def get_new_song(self):
        """Block until a new song selection is received."""

    def get_payment(self):
        """Block until payment has been received."""
        if self.song_cost.amount == 0:
            return FreePayment()

    def display_playing(self, cd, song):
        """Display the currently playing song to the user."""


class JukeboxError(Exception):
    """An error encountered in a jukebox.

    Not all errors are unexpected -- some should be caught.
    """
    pass


class SongNotPlayedError(JukeboxError):
    """These are the errors that result in a song not being played.

    Callers to append_song should catch this and be sure
    to refund or cache the payment if it is raised.
    """

class PaymentIncorrect(SongNotPlayedError):
    pass


class CdNotFoundError(SongNotPlayedError):
    pass


class SongNotFoundError(SongNotFoundError):
    pass

class Payment(object):
    """A payment handler.

    This object existing means that:
    - the payment of `amount` has been made.
    - the pyament has been _accepted_.
    """
    def __init__(self, amount: Cost):
        self.amount = amount

    @abstractmethod
    def refund(self):
        pass


class CreditCardPayment(Payment):
    pass # ...


class CashPayment(Payment):
    pass # ...


class FreePayment(Payment):
    """A non-payment. Amount==0, only works when jukebox is in free mode."""
    pass


class Cd(object):
    def __init__(self, name: str, songs: List[Song]):
        self.name = name
        self.songs = {s.name: s for s in songs}


class Song(object:
    def __init__(self, name, data):
        self.name = name
        self.data = data

    def play(self):
        """Play the song."""
```


