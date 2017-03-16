# !/bin/python 
#
# File: test-all.py
# Author: Eric Kingsley
#
# Simple tester file for IR Generation of the GLSL Compiler
#
import os
from subprocess import *

TEST_DIRECTORY = 'public_samples'


def main(): 
  totalTests = 0
  testResults = 0
  # make the program yo
  makeResult = Popen("make", shell = True, stderr = STDOUT)
  if (makeResult.wait() != 0): 
    print '\033[0;36m' + 'FATAL ERROR' + '\033[0;0m' + ': make failed to compile'
    return

  for _, _, files in os.walk(TEST_DIRECTORY):
    for file in files:
      if not (file.endswith('.glsl') or file.endswith('.frag')):
        continue
      totalTests += 1
      testName = file.split('.')[0]
      refName = os.path.join(TEST_DIRECTORY, '%s.%s' % (testName, "out"))
      if not (checkFileExt(refName)): continue
      datName = os.path.join(TEST_DIRECTORY, '%s.%s' % (testName, "dat"))
      if not (checkFileExt(datName)): continue
      # Run that test like you mean it
      testResults += runTest(file, refName)
  printTestResults(testResults, totalTests)
  # Perform cleanup on binary files
  Popen("rm -rf " + os.path.join(TEST_DIRECTORY, "*.bc"), shell = True)

def checkFileExt(name):
  if not (os.path.isfile(name)):
    print '\033[0;36m' + 'FILE ERROR' + '\033[0;0m' + ': ".%s" file does not exist for test "%s"' % (name.split('.')[1], name)
    print 'Skipping execution...\n'
    return False
  return True

def run(test, log):
  fullPath = os.path.join(TEST_DIRECTORY, test)
  ret = Popen('./glc < ' + fullPath, shell = True, stderr = STDOUT, stdout = log)
  ret.wait()
  return ret

def runTest(glslFile, outFile):
  bcFile = os.path.join(TEST_DIRECTORY, '%s.bc' % glslFile.split('.')[0])
  with open(bcFile, "w") as log:
    glcResult = run(glslFile, log)
  gliResult = Popen('./gli ' + bcFile, shell = True, stderr = STDOUT, stdout = PIPE)
  result = Popen('diff -w - ' + outFile, shell = True, stdin = gliResult.stdout, stdout = PIPE)
  return colorifyTestResult(glslFile, result)

def colorifyTestResult(glslFile, result): 
  output = ''.join(result.stdout.readlines())
  print '++ Executing %s' % (glslFile)
  if not (output):
    print 'Result: ' + '\033[1;32m' + 'PASS' + '\033[0;0m' + '\n'
    return 1
  else:
    print output
    print 'Result: ' + '\033[0;36m' + 'FAIL' + '\033[0;0m' + '\n'
    return 0

def printTestResults(results, total):
  print 'SCORE: ' + '\033[1;37m' + str(results) + '/' + str(total) + '\033[0;0m'


# entry point
main()
