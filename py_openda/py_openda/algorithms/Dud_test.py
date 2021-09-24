#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 11:06:08 2018

@author: hegeman
"""

import unittest
from py_openda.algorithms import Dud

def f(p):
    return [p[0], p[1]]

class TestDud(unittest.TestCase):
    
    def test_dud(self):
        (total_cost, p_opt, hist) = Dud.dud(f, p_old=[2, 7], p_pert=[0.1, 0.1], obs=[1, 33], std=[1.0, 1.0])


        self.assertAlmostEqual(total_cost, 0.0)
        self.assertAlmostEqual(p_opt[0], 1.0)
        self.assertAlmostEqual(p_opt[1], 33.0)


if __name__ == '__main__':
    unittest.main()
