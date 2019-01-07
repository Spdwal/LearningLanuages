import collections

Card = collections.namedtuple('Cadr', ['rank', 'suit'])

class FrenchDeck:
    ranks = [str(n) for n in range(2, 11)] + list('JQKA')
    suits = 'spades diamonds clubs hearts'.split()

    def __init__(self):
        self.__cards = [Card(rank, suit) for suit in self.suits
                        for rank in self.ranks]


    def __len__(self):
        return len(self.__cards)


    def __getitem__(self, position):
        return self.__cards[position]

suit_values = dict(spades = 3, hearts = 2, diamonds = 1, clubs = 0)

def spades_high(card):
    rank_value = FrenchDeck.ranks.index(card.rank)
    return rank_value * len(suit_values) + suit_values[card.suit]
