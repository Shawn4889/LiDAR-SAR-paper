# Author: Xiaoxuan Li

import pandas as pd
import numpy as np
from mlxtend.preprocessing import TransactionEncoder
from mlxtend.frequent_patterns import apriori
from sklearn.neighbors import KNeighborsClassifier


def frequent():
    dataset = [
    ['A', 'B', 'C'],
    ['A', 'B', 'C', 'F'],
    ['A', 'D'],
    ['A', 'D', 'E'],
    ['B'],
    ['B', 'C', 'E'],
    ['C', 'E'],
    ['D', 'E'],
    ['F']
    ]
    te = TransactionEncoder()
    te_ary = te.fit(dataset).transform(dataset)
    df = pd.DataFrame(te_ary, columns=te.columns_)
    print(df)
    #m = apriori(df, min_support=0.05, use_colnames=True)
    m = apriori(df, min_support=0.01, use_colnames=True)
    print(m)

def fptree():
    dataset = [
        ['A', 'B', 'E'],
        ['A', 'B'],
        ['A', 'B', 'C'],
        ['A', 'D'],
        ['C', 'D'],
    ]

    te = TransactionEncoder()
    te_ary = te.fit(dataset).transform(dataset)
    df = pd.DataFrame(te_ary, columns=te.columns_)
    print(df)
    # m = apriori(df, min_support=0.05, use_colnames=True)
    m = apriori(df, min_support=0.4, use_colnames=True)
    print(m)




frequent()
#fptree()
