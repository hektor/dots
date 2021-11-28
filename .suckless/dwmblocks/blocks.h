//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
  /*Icon*/  /*Command*/   /*Update Interval*/ /*Update Signal*/
/*  {"Mem:", "free -h | awk '/^Mem/ { print $3\"/\"$2 }' | sed s/i//g", 30,   0}, */
  {"", "cat /tmp/pomo",             1,  0},
  {"Week ", "date '+%V'",           60, 0},
  {"", "check-battery-level",       60, 0},
  {"", "date '+%a %d %b %I:%M%p'",  5,  0},
  {"", "task status:pending count", 20, 0}
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;
