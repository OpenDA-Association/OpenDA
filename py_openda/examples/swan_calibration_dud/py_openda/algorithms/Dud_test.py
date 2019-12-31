#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 11:06:08 2018

@author: hegeman
"""

import unittest
import Dud

def f(p):
    return [p[0], p[1]]

a= Dud.dud(f, [2, 7], [1, 33], [1,1])

class TestDud(unittest.TestCase):
    
    def test_dud(self):

        self.assertAlmostEqual(a[0], 0)
        self.assertAlmostEqual(a[1][0],1)
        self.assertAlmostEqual(a[1][1],33)


if __name__ == '__main__':
    unittest.main()