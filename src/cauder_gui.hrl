-record(status, {loaded  = false,
                 running = false}).

-define(FRAME_SIZE_INIT, {750, 600}).
-define(FRAME_SIZE_MIN,  {750, 600}).
-define(FRAME_SIZE_MAX,  {750, 600}).

-define(FONT_SIZES, [8, 10, 12, 13, 14, 16, 20, 24, 32, 36, 42, 48]).

-define(ABOUT,    ?wxID_ABOUT).
-define(EXIT,     ?wxID_EXIT).
-define(OPEN,     ?wxID_OPEN).
-define(ZOOM_IN,  100).
-define(ZOOM_OUT, 101).

-define(START_BUTTON,        400).

-define(FORW_INT_BUTTON,     410).
-define(FORW_SCH_BUTTON,     411).
-define(BACK_INT_BUTTON,     412).
-define(BACK_SCH_BUTTON,     413).

-define(FORWARD_BUTTON,    422).
-define(BACKWARD_BUTTON,   423).
-define(NORMALIZE_BUTTON,  424).
-define(ROLL_BUTTON,       425).

-define(ROLL_SEND_BUTTON,  426).
-define(ROLL_SPAWN_BUTTON, 427).
-define(ROLL_REC_BUTTON,   428).
-define(ROLL_VAR_BUTTON,   429).

-define(SYSTEM,         500).
-define(STATUS,         501).
-define(FRAME,          502).
-define(INPUT_TEXT,     510).
-define(PID_TEXT,       511).
-define(STEP_TEXT,      512).
-define(STATE_TEXT,     513).
-define(CODE_TEXT,      514).
-define(ROLL_PID_TEXT,  515).
-define(ROLL_STEP_TEXT, 516).
-define(STATUS_BAR,     520).
-define(INPUT_SIZER,    530).
-define(FUN_CHOICE,     531).
-define(LEFT_NOTEBOOK,  540).
-define(RIGHT_NOTEBOOK, 541).
-define(RBOT_NOTEBOOK,  542).
-define(TRACE_TEXT,     550).
-define(ROLL_LOG_TEXT,  551).

-define(ROLL_SEND_ID_TEXT,  560).
-define(ROLL_SPAWN_ID_TEXT, 561).
-define(ROLL_REC_ID_TEXT,   562).
-define(ROLL_VAR_ID_TEXT,   563).

-define(PAGEPOS_CODE,  0).
-define(PAGEPOS_STATE, 1).
-define(PAGEPOS_MANU,  0).
-define(PAGEPOS_SEMI,  1).
-define(PAGEPOS_AUTO,  2).

-define(NULL_LABEL, null_label).

-define(INFO_TEXT, "A Causal-consistent Debugger for Erlang. More info at: https://github.com/mistupv/cauder").
-define(ERROR_NUM_STEP, "The number of steps is not correct.").
-define(ERROR_NUM_ARGS, "The number of arguments is not correct.").
