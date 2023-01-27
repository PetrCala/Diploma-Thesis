import numpy as np
import random
import re
import time
import math
import sys


import cv2
from ctypes import windll
from PIL import ImageGrab
from pynput.keyboard import Key, Controller, GlobalHotKeys, Events, HotKey, Listener, KeyCode
import win32.win32gui as win32gui

from input import Input
from directkeys import click, queryMousePosition, moveMouseTo
windll.user32.SetProcessDPIAware() #Make windll properly aware of your hardware
keyboard = Controller()


I = Input()


def saveCitation():
    scholar = I.getWindowHwnd("ability bias")
    win32gui.SetForegroundWindow(scholar)
    I.clickScreen()
    time.sleep(0.5)
    I.moveClick(831, 603)
    time.sleep(0.4)
    pressHotkey(Key.ctrl, 'c', sleep = 0.5)
    I.useKey(Key.esc, sleep = 0.2)
    # Focus excel
    excel = I.getWindowHwnd("Literature - Excel")
    win32gui.SetForegroundWindow(excel)
    pressHotkey(Key.ctrl, 'v', sleep = 0.2)
    I.useKey(Key.enter)
    win32gui.SetForegroundWindow(scholar)
    print('Study citation extracted successfully')
    return True

def function_2():
    '''Stop the listener.
    '''
    print('Function 2 activated')

def function_3():
    print('Function 3 activated')

def pressHotkey(key1, key2, sleep = True):
    '''Press a combination of HotKeys, such as <ctrl>+c.
    '''
    I.useKey(key1, method='press', sleep=sleep)
    I.useKey(key2, sleep=False)
    I.useKey(key1, method='release', sleep=False)
    return None


hotkey1 = HotKey(
    [Key.alt, KeyCode(char='q')],
    saveCitation
)

hotkey2 = HotKey(
    [Key.ctrl, Key.alt, KeyCode(char='d')],
    function_2
)

hotkey3 = HotKey(
    [Key.ctrl, Key.alt, KeyCode(char='f')],
    function_3
)

hotkeys = [hotkey1, hotkey2, hotkey3]


def signal_press_to_hotkeys(key):
    for hotkey in hotkeys:
        hotkey.press(l.canonical(key))

def signal_release_to_hotkeys(key):
    for hotkey in hotkeys:
        hotkey.release(l.canonical(key))
    # Stop the loop
    if KeyCode(char='`') == key:
        return False

with Listener(on_press=signal_press_to_hotkeys, on_release=signal_release_to_hotkeys) as l:
    l.join()