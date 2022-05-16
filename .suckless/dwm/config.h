/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int gappx     = 16;       /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Iosevka Term SS08:size=12" };
static const char dmenufont[]       = "Iosevka Term SS08:size=14";
static const char col_gray0[]       = "#0a0a0a";
static const char col_gray1[]       = "#111111";
static const char col_gray2[]       = "#333333";
static const char col_gray3[]       = "#555555";
// Warn & urgent colors
static const char col_gray4[]       = "#cccccc";
static const char col_black[]       = "#000000";
static const char col_red[]         = "#ff0000";
static const char col_yellow[]      = "#ffff00";
static const char col_white[]       = "#ffffff";

static const char *colors[][3]      = {
	/*                fg         bg         border   */
	[SchemeNorm] =  { col_gray4, col_gray0, col_gray2 },
	[SchemeSel]  =  { col_white, col_gray0,  col_gray3 },
  // Extra color schemes
  [SchemeWarn] =	{ col_black, col_yellow, col_red },
  [SchemeUrgent]=	{ col_white, col_red,    col_red },
};

/* tagging */
static const char *tags[] = { "α", "β", "γ", "δ", "ε" };

static const Rule rules[] = {
	/* xprop(1):
	 *  WM_CLASS(STRING) = instance, class
	 *  WM_NAME(STRING) = title
	 */
	/* class            instance  title  tags mask  isfloating  monitor */
	{ NULL,             NULL,     NULL,  0,         False,      -1 },
	{ "obsidian",       NULL,     NULL,  1 << 0,    False,      -1 },
	{ "Brave-browser",  NULL,     NULL,  1 << 1,    False,      -1 },
	{ "Zathura",        NULL,     NULL,  1 << 3,    False,      -1 },
	{ "KeePassXC",      NULL,     NULL,  1 << 4,    False,      -1 },
	{ "Anki",           NULL,     NULL,  1 << 4,    False,      -1 },
	{ "Goodvibes",      NULL,     NULL,  1 << 4,    False,      -1 },
	{ "Com.github.xournalpp.xournalpp",  NULL,  NULL,  0,  True,  -1 },
};

/* layout(s) */
static const float mfact     = 0.62; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
	{ "|M|",      centeredmaster },
	{ ">M>",      centeredfloatingmaster },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu-run-history", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_gray1, "-sf", col_gray4, NULL };
static const char *termcmd[]  = { "st", NULL };
static const char *prtscrcmd[] = { "flameshot", "gui", NULL};
static const char *incvol[] = { "pactl", "set-sink-volume", "56", "+5%", NULL };
static const char *decvol[] = { "pactl", "set-sink-volume", "56", "-5%", NULL };
static const char *mutevol[] = { "pactl", "set-sink-mute", "56", "toggle", NULL };
static const char *incbrightness[] = { "xbacklight", "-inc", "5", NULL };
static const char *decbrightness[] = { "xbacklight", "-dec", "5", NULL };

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ NULL,                         XK_Print,  spawn,          {.v = prtscrcmd } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_u,      setlayout,      {.v = &layouts[3]} },
	{ MODKEY|ShiftMask,             XK_u,      setlayout,      {.v = &layouts[4]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ MODKEY,                       XK_minus,  setgaps,        {.i = -5 } },
	{ MODKEY,                       XK_equal,  setgaps,        {.i = +5 } },
	{ MODKEY|ShiftMask,             XK_equal,  setgaps,        {.i = 0  } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
	{ NULL,                         XF86XK_AudioLowerVolume, spawn, {.v = decvol } },
	{ NULL,                         XF86XK_AudioRaiseVolume, spawn, {.v = incvol } },
	{ NULL,                         XF86XK_AudioMute,        spawn, {.v = mutevol } },
	{ NULL,                         XF86XK_MonBrightnessUp,  spawn, {.v = incbrightness} },
	{ NULL,                         XF86XK_MonBrightnessDown, spawn, {.v = decbrightness} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
