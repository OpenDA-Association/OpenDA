#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 11:00:00 2018

@author: hegeman
"""

import unittest
import Kwadratic

class TestKwadratic(unittest.TestCase):
    
    def test_array(self):
        a = py_list_to_j_array([1,2,3])
        b = py_list_to_j_array([1.0,2.0,3.0])
        self.assertEqual(j_array_to_py_list(a), j_array_to_py_list(b))

if __name__ == '__main__':
    unittest.main()