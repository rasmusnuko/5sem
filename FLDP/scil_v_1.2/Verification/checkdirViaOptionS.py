#!/usr/bin/env python3

from sys import argv, stdout
import os, os.path
import time
import getopt

ProgAndSolDir = ""

def fileempty(filename):
   file = open(filename, "r")
   text = file.read()
   file.close()
   return len(text) == 0
   
#
# get maximum number of open files
#
MAXFD = os.sysconf('SC_OPEN_MAX')

#
# run a program until completion or timeout.
#
def execvp_wait(path, args, timeout):
    global TimedOut
    # fork child, closing all file descriptors
    pid = os.fork()
    if pid==0: # child
        # close all open file descriptors except
        # stdin,stdout & stderr
        for fd in range(3,MAXFD):
            try: os.close(fd)
            except os.error: pass

        # TODO: connect stdin,stdout & stderr to
        # something reasonable
        # exec the child
        try:
            os.execvp("./" + path,args)
        except Exception:
            # print traceback if exception occurs
            import traceback
            traceback.print_exc(file=os.sys.stderr)
        # always exit
        os._exit(1)
    else: # parent

        t = timeout
        while t>=0:
            # wait for completion
            child_pid,status = os.waitpid(pid, os.WNOHANG)
            if child_pid==pid:
                if os.WIFSTOPPED(status) or \
                   os.WIFSIGNALED(status):
                    return None
                elif os.WIFEXITED(status):
                    return os.WEXITSTATUS(status)
            # wait for a second
            time.sleep(0.2)
            t -= 1

        # not completed within timeout seconds
        TimedOut = 1
        os.kill(pid, 9)
        os.kill(pid+1, 9)

from os import walk
#
# Test
#
if __name__=="__main__":
    TimedOut = 0
    compiler = argv[1]
    path = argv[2]
    user = str(os.getuid())

    f = []
    for (dirpath, dirnames, filenames) in walk(path):
       ProgAndSolDir = dirpath + "/"
       f.extend(filenames)

    for index in range(len(filenames)):
       TimedOut = 0
       extension = os.path.splitext(filenames[index])[1][1:]
       filename = os.path.splitext(filenames[index])[0];
       if extension != "src":
         continue

       stdout.write("\n")
       stdout.write("Checking: " + filename + "\n")
       stdout.write("Pretty.......")
       stdout.flush()
       code = execvp_wait("reproduce",
                       ["reproduce", compiler + " -s",
                        ProgAndSolDir + filename + ".src", user], 60)
       if TimedOut:
           stdout.write("compiler TIMED OUT")
       else:
           if code != 0:
               stdout.write("[compiler ERROR code " + str(code) + "]")
           else:
              stdout.write("Done")
       stdout.write("\n")
    
       stdout.write("Compiling....")
       stdout.flush()
       code = execvp_wait("checkwrap",
                       ["checkwrap", compiler,
                        "/tmp/" + user + "-reproduced.src", user], 60)
       if TimedOut:
           stdout.write("compiler TIMED OUT")
       else:
           if code != 0:
               stdout.write("[compiler ERROR code " + str(code) + "]")
           else:
              stdout.write("Done")
       stdout.write("\n")
    
       if not TimedOut and code == 0:
           stdout.write("Assembling...")
           stdout.flush()
           code = execvp_wait("gccwrap", ["gccwrap", user], 60)
           stdout.write("Done")
           if code != 0:
               stdout.write(" [assembler ERROR code " + str(code) + "]")
           stdout.write("\n")

           TimedOut = 0
           stdout.write("Executing....")
           stdout.flush()
           code = execvp_wait("execwrap", ["execwrap", user], 60)
           if TimedOut:
               stdout.write("runtime TIMED OUT")
           else:
               if code == 0:
                   stdout.write("Done")
                   expFile = ProgAndSolDir + filename + ".eop"
                   if os.path.isfile(expFile):
                       os.system("diff /tmp/" + user + "-output " + \
                                 expFile + " > /tmp/" + \
                                 user + "-difference")
                       if not fileempty("/tmp/" + user + "-difference"):
                          stdout.write("\nINCORRECT output")
                       else:
                          stdout.write("\nCorrect output")
                   else:
                       stdout.write("\nFile with expected output did not exist")
               else:
                   stdout.write("[runtime ERROR code " + str(code) + "]")
           stdout.write("\n")
