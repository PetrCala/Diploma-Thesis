#----- Script for handling the automatic computer input -----
import numpy as np
import random
import re
import time
import math
import sys


import cv2
from ctypes import windll
from PIL import ImageGrab
from pynput.keyboard import Key, Controller
import pytesseract
import win32.win32gui as win32gui

from directkeys import click, queryMousePosition, moveMouseTo
windll.user32.SetProcessDPIAware() #Make windll properly aware of your hardware
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files (x86)\Tesseract-OCR\tesseract' # Pytesseract path
tessdata_dir_config = r'--tessdata-dir "C:\Program Files (x86)\Tesseract-OCR\tessdata"'
keyboard = Controller()

class classproperty(property):
    def __get__(self, cls, owner):
        return classmethod(self.fget).__get__(None, owner)()

class Input():
    def __init__(self,hwnd:int = None):
        self.screen_pos = self.monitor_coords if hwnd is None else self.getWindowCoords(hwnd) 

    @property
    def numbers(self):
        '''A list of the 10 roman numbers as strings. Used for keyboard input.
        '''
        return [str(i) for i in range(11)]

    def calculateCoords(self, coords:list, from_scale:bool = True, screen_pos_:list = None):
        '''Input a list of scale coordinates and return a list of the actual coordinates
        for the user's screen. It is possible to calculate in reverse direction too.
        :args:
            scale_coords[list] - A list of two scale coordinates marking a certain point on
                the screen.
            from_scale[bool, optional] - If True, input scale coordinates and return the actual
                coordinates on user's screen. If False, do the inverse. Defaults to True.
            screen_pos_[list, opional] - List of coordinates, indicating the screen, for which
                the calculation should be done. Defaults to None, in which case the screen_pos
                attribute of the last class is used.
            
        :note:
            Scale coordinates - An initial point of [0.5,0.5] marks a point in the middle of the screen.
                In other words, it is 50 percent from top left corner in either direction.
            Actual coordinates - Actual pixels of the screen, such as [1000,500].
            Also assumes the game covers all of the screen, and does not move. Might change later.
        '''
        if not len(coords) == 2:
            raise ValueError('The coordinates must be input as a list of length 2')
        x_inp, y_inp = coords
        current_screen = self.screen_pos if screen_pos_ is None else screen_pos_ 
        screen_width = current_screen[2] - current_screen[0]
        screen_height = current_screen[3] - current_screen[1]
        if from_scale:
            # Take left/top edge, and add the desired distance (percentage of screen) to get coord
            x = int(current_screen[0] + screen_width * x_inp) # x axis
            y = int(current_screen[1] + screen_height * y_inp) # y axis
        else:
            # Take pixels from edge to coord, then calculate what percentage of screen
            # that distance covers
            x = round((x_inp - current_screen[0])/screen_width, 3)
            y = round((y_inp - current_screen[1])/screen_height, 3)
        return [x, y]

    def rangeToPixels(self, range:list):
        '''Specify a list of 4 scale coordinates and return a list of four points,
        which define (in pixels) the top left and bottom right points
        of the range, respectively.
        Args:
            range (list): List of four points of the range, in scale.
        '''
        if not len(range) == 4:
            raise ValueError('You must specify the range as a list of four points')
        start_ = range[0:2]
        end_ = range[2:4]
        start = self.calculateCoords(start_)
        end = self.calculateCoords(end_)
        return start + end

    def readTextInRange(self, range:list, lang:str = 'ces', view_range:bool = False):
            '''Specify as a list of scale coordinates the range in which
            a text should be recognized and return the text as a string.
            Args:
                range (list) - A list of scale coordinates.
                lang (str) - Language of the text in the range. Defaults to 'ces' (Czech).
                view_range (bool, optional) - If True, also open the screen.
                    Defaults to False.
            '''
            range_pixels = self.rangeToPixels(range)
            img = self.createScreen(range_pixels, color_scale='orig')
            if view_range:
                self.openScreen(range_pixels, color_scale = 'orig')
            return pytesseract.image_to_string(img, lang = lang, config = tessdata_dir_config)

    def createScreen(self, screen_pos:list = None, color_scale = 'gray'):
            '''Return a numpy array representing pixels on a screen. Specify the range
            of the screen with "screen_pos".
            :args:
                screen_pos[list] - A list of 4 integers specifying the range where
                    the screen should be taken. Defaults to None (whole screen).
                color_scale[str] - Color scale which the screenshot should take.
                    Can be set to 'gray', 'orig'.
            '''
            if screen_pos is None:
                screen_pos = self.screen_pos # Default to the whole screen
            if not len(screen_pos) == 4:
                raise ValueError('The screen_pos argument must be a list of length 4')
            screen = np.array(ImageGrab.grab(bbox=screen_pos))
            if color_scale == 'gray':
                return cv2.cvtColor(screen, cv2.COLOR_BGR2GRAY)
            elif color_scale == 'orig':
                return cv2.cvtColor(screen, cv2.COLOR_BGR2RGB) #Original color scale
            raise ValueError('The color_scale argument is misspecified.')

    def openScreen(self, screen_pos:list = None, win_name:str = 'Ekura screenshot', color_scale = 'gray'):
        '''Open the screenshot for viewing.
        :args:
            win_name[str] - Name of the window.
            screen_pos[list, optional] - List of coordinates where the screenshot
                should be taken. If None, use the whole game screen. Defaults to None.
        '''
        scale = 1
        if screen_pos is None:
            screen_pos = self.screen_pos
            scale = 0.9 # Resize if fullscreen
        screen = self.createScreen(screen_pos, color_scale=color_scale) #Take a screenshot
        window_width = screen_pos[2] - screen_pos[0]
        window_height = screen_pos [3] - screen_pos[1]
        window_res = [int(window_width*scale), int(window_height*scale)] # Window resizing
    
        cv2.namedWindow(win_name, cv2.WINDOW_NORMAL) # Create a Named Window
        cv2.moveWindow(win_name, 0, 0) # Move it to (X,Y)
        cv2.imshow(win_name, screen) # Show the Image in the Window
        cv2.resizeWindow(win_name, window_res[0], window_res[1])   # Resize the Window
        cv2.waitKey(0); cv2.destroyAllWindows(); cv2.waitKey(1) #Handle closing of the window
        return None

    def getMousePosition(self, scale = False, verb = False):
        '''Get the coordinates of the current mouse position.
        
        Arg:
            scale [bool] - If True, return/print the coordinates as a scale.
            verb [bool] - If True, only print out the output. If False, return the output
                as a list of coordinates.
        '''
        m = queryMousePosition()
        x = m.x
        y = m.y
        if scale:
            x, y = self.calculateCoords([x,y], from_scale = False) # Absolute coordinates to scale
        if verb:
            print(f'The mouse position is\nx:{x}\ny:{y}')
            return None
        return x,y

    def num_key(self, key):
        '''Convert the string corresponding to a roman number to a key code legible by the keyboard.
        '''
        if not key in self.numbers:
            raise ValueError('Only roman numbers can be converted.')
        return keyboard._KeyCode(char = key)
        
    def useKey(self, key, method:str = 'tap', sleep:bool = True):
            '''An extended method for handling more complex key pressing. Used to input only one key.
            :arg:
                key - Key to be pressed. Accepts all inputs of pynput, along with roman numbers in a string form (i.e. '5').
                method[str] - Method by which the key shall be used. Must be an attribute of the keyboard.
                sleep[bool] - If true, insert a 0.2 sleep time after the key press. Defaults to True.
            '''
            if not hasattr(keyboard, method):
                raise ValueError('You are trying to perform an invalid operation on the keyboard.')
            if key in self.numbers:
                key = self.num_key(key) #Parse a roman number
            getattr(keyboard, method)(key) #Tap, press,... the key
            if sleep:
                period = 0.7 if sleep is True else sleep
                time.sleep(period)
            return None

    def useKeys(self, keys, sleep = False, sleep_after = False):
        '''For each key in keys, press this key. Keys can be any iterable object.
        :arg:
            keys - Keys to be pressed/written.
            sleep (bool) - If True, insert a small wait time in between key presses.
                Also accepts float, which specifies the wait time. Defaults to False.
            sleep_after (bool) - Similar to sleep, only for the sleep timer after all
                keys were pressed.
        '''
        for key in keys:
            self.useKey(key, sleep = sleep)
        if sleep_after:
            period = 0.7 if sleep_after is True else sleep_after
            time.sleep(period)
        return True

    def getWindowCoords(self, hwnd:int):
        '''Return the coordinates of the game window as a list of 4 coordinates.
        :arg:
            hwnd (int) - Handle number of the window to assess.
        '''
        pos = win32gui.GetWindowPlacement(hwnd)
        return list(pos[4])

    def moveClick(self, x, y, from_scale=False):
        '''Specify coordinates and click there. Used for clicks in game,
        automaically adds a small wait window in order to guarantee the click.
        Returns the cursor back to the starting position.
        :arg:
            from_scale(bool) - If True, input coordinates in scale.
        '''
        pos = queryMousePosition()
        x_, y_ = pos.x, pos.y # Initial mouse coordinates
        if from_scale:
            [x, y] = self.calculateCoords([x, y], screen_pos_=self.screen_pos)
        moveMouseTo(x, y)
        time.sleep(0.1) # Allow for cursor positioning
        click(x, y)
        moveMouseTo(x_,y_) # Return the mouse
        return None

    @staticmethod
    def checkStringForMatches(input_string:str, match_list:list, verbose:bool = False):
        '''Input a string, and a list of words to look for, and return the number
        of matches found in the string for said list.
        Args:
            input_string (str): String to search for potential matches.
            match_list (list): List of words to search for in the string.
            verbose (bool): If True, print out a message with the number of matches.
        Returns:
            int: The number of matches found in the string.
        '''
        matches = 0
        input_words = input_string.split()
        for word in input_words:
            if word in match_list:
                matches += 1
        if verbose:
            plural = 'es' if matches != 1 else ''
            print(f'Found {matches} match{plural}.')
        return matches

    @staticmethod
    def mouseOnScreen(screen_pos):
        '''Specify the screen position as a list of coordinates and check whether the mouse is within these coordinates.
        Args:
            screen_pos ([type]): [description]
        Returns:
            bool: True if the mouse is within the specified coordinates, False otherwise.
        '''
        assert isinstance(screen_pos, list) and len(screen_pos) == 4, 'The screen element is misspecified'
        pos = queryMousePosition() #Get mouse position
        on_screen = screen_pos[0] < pos.x < screen_pos[2] and screen_pos[1] < pos.y < screen_pos[3]
        return True if on_screen else False

    @classproperty
    def monitor_coords(cls):
        '''Coordinates of the monitor, which the game is allowed to be ran on.
        '''
        return [0,0] + cls.screen_size

    @classproperty
    def screen_size(cls):
        '''
        A static property defining the screen resolution.
        
        :return:
            list: A list of two coordinates marking the bottom right part of the screen.
        '''
        if sys.platform[0:3] == 'win':
            user32 = windll.user32
            return [user32.GetSystemMetrics(0), user32.GetSystemMetrics(1)]
        else:
            raise SystemError('The code must be ran on the Windows platform')

    @staticmethod
    def clickScreen():
        '''Click where the cursor is pointing.
        '''
        pos = queryMousePosition() # Query cursor position
        click(pos.x, pos.y) # Click
        return None

    @staticmethod
    def randomizeClicking(x,y):
        '''Input the two coordinates and move each one by a small, random amount of pixels
        in a random direction. Return the new coordinates as a pair.
        '''
        scale1 = random.uniform(-5,5)
        scale2 = random.uniform(-5,5)
        x_ = int(x + scale1)
        y_ = int(y + scale2)
        return x_, y_

    @staticmethod
    def getWindowHwnd(lookup_words):
        '''Specify a window name and return its window handle. Allows regex patterns.
        :arg:
            lookup_words (str, or list) - Word/s that the window name must contain.
        :return:
            hwnd -  Handle of the said window.
        '''
        windows = []
        def callback(hwnd, extra):
            if isinstance(lookup_words, str):
                if lookup_words.upper() in win32gui.GetWindowText(hwnd).upper():
                    windows.append(hwnd)
            elif isinstance(lookup_words, list):
                if all([word.upper() in win32gui.GetWindowText(hwnd).upper() for word in lookup_words]):
                    windows.append(hwnd)
            else:
                raise ValueError('Specify either a single or multiple words that the window should contain.')
            return True
        win32gui.EnumWindows(callback, None)
        if windows == []: #Window not found
            return None
        elif len(windows) > 1:
            raise ValueError('Multiple windows open.')
        return windows[0]

    @staticmethod
    def dist(x1, y1, x2, y2):
        return math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)