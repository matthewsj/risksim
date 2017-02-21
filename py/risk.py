from collections import defaultdict
from itertools import chain
from functools import lru_cache
import csv
import sys

AMMO_SHORTAGE = False
BUNKER = True

class ProbabilityTable(object):
    def __init__(self, ammo_shortage=False, bunker=False):
        self.ammo_shortage = ammo_shortage
        self.bunker = bunker
        self._skirmish_outcomes = self._ComputeSkirmishOutcomeTable()

    def _ComputeSkirmishOutcomeTable(self):
        table = defaultdict(dict)
        for a in range(1, 4):
            for d in range(1, 3):
                table[a][d] = self._ComputeOutcomes(a, d)
        return table

    def _ComputeOutcomes(self, a, d):
        attacker_dice = list(self._AllRollsForNDice(a))
        defender_dice = list(self._AllRollsForNDice(d))

        outcomes = {}
        n = 0
        for i in attacker_dice:
            for j in defender_dice:
                s = self._Score(i,j)
                n += 1
                if s in outcomes:
                    outcomes[s] += 1
                else:
                    outcomes[s] = 1

        result = {}
        result['outcomes'] = outcomes
        result['total_outcomes'] = n
        return result

    def _Score(self, a, d):
        sorted_a = sorted(a, reverse=True)
        sorted_d = sorted(d, reverse=True)

        if self.ammo_shortage:
            sorted_d[0] = max(sorted_d[0] - 1, 1)
            sorted_d = sorted(sorted_d, reverse=True)
        elif self.bunker:
            sorted_d[0] = min(sorted_d[0] + 1, 6)
            sorted_d = sorted(sorted_d, reverse=True)

        if len(a) < len(d):
            m = len(a)
        else:
            m = len(d)
        attacker_losses = 0
        defender_losses = 0
        for i in range(0,m):
            if sorted_a[i] > sorted_d[i]:
                defender_losses = defender_losses + 1
            else:
                attacker_losses = attacker_losses + 1
        return (attacker_losses, defender_losses)


    def _AllRollsForNDice(self, n):
        if n == 0:
            return [[]]
        else:
            smaller_rolls = self._AllRollsForNDice(n-1)
            return _flatmap(lambda rest: map(lambda first: [first] + rest, [1,2,3,4,5,6]),
                            smaller_rolls)

    @lru_cache(maxsize = None)
    def ComputeWinPercent(self, a, d):
        if a == 0:
            return 0
        elif d == 0:
            return 1
        else:
            attacker_dice = min(a, 3)
            defender_dice = min(d, 2)

            x = self._skirmish_outcomes[attacker_dice][defender_dice]
            win_percent = 0
            for (attackers_loss, defenders_loss), count in x['outcomes'].items():
                win_percent += ((count / x['total_outcomes'])
                                * self.ComputeWinPercent(a - attackers_loss,
                                                         d - defenders_loss))
            return win_percent


    def Table(self, max_attackers, max_defenders):
        result = []
        for i in range(1, max_attackers+1):
            row = []
            for j in range(1, max_defenders+1):
                row.append('{0:.3f}'.format(self.ComputeWinPercent(i,j)))
            result.append(row)
        return result


def _flatmap(f, items):
    return chain.from_iterable(map(f, items))


if __name__ == '__main__':
    normal = ProbabilityTable()
    bunkers = ProbabilityTable(bunker=True)
    ammo_shortage = ProbabilityTable(ammo_shortage=True)

    def PrintTable(name, probability_table):
        print('{0}:'.format(name))
        t = probability_table.Table(25, 25)
        writer = csv.writer(sys.stdout, delimiter='\t')
        writer.writerows(t)

    PrintTable('Base probability', normal)
    PrintTable('Bunkers', bunkers)
    PrintTable('Ammo shortage', ammo_shortage)
