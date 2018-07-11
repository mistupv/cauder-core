-module(cauder_gui).
-export([setup_gui/0]).

-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").

setup_gui() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, ?APP_STRING, [{size, ?FRAME_SIZE_INIT}]),
  ref_start(),
  ref_add(?FILE_PATH, "."),
  ref_add(?STATUS, #status{}),
  ref_add(?FRAME, Frame),
  setupMenu(),
  wxFrame:createStatusBar(Frame, [{id, ?STATUS_BAR}]),
  wxEvtHandler:connect(Frame, close_window),
  wxEvtHandler:connect(Frame, command_button_clicked),
  wxEvtHandler:connect(Frame, command_menu_selected),
  wxEvtHandler:connect(Frame, command_text_updated),
  setupMainPanel(Frame),
  wxFrame:show(Frame),
  loop(),
  utils_gui:stop_refs(),
  ref_stop().

setupMainPanel(Parent) ->
  MainPanel = wxPanel:new(Parent),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],

  LeftPanel = wxPanel:new(MainPanel),
  LeftSizer = setupLeftSizer(LeftPanel),
  wxWindow:setSizerAndFit(LeftPanel, LeftSizer),

  RightPanel = wxPanel:new(MainPanel),
  RightSizer = setupRightSizer(RightPanel),
  wxWindow:setSizerAndFit(RightPanel, RightSizer),

  wxSizer:add(MainSizer, LeftPanel, SizerFlags),
  wxSizer:add(MainSizer, RightPanel, SizerFlags),
  wxWindow:setSizer(MainPanel, MainSizer),
  MainPanel.

setupLeftSizer(Parent) ->
  Notebook = wxNotebook:new(Parent, ?LEFT_NOTEBOOK),
  ref_add(?LEFT_NOTEBOOK, Notebook),
  CodePanel = setupCodePanel(Notebook),
  StatePanel = setupStatePanel(Notebook),
  wxNotebook:addPage(Notebook, CodePanel, "Code"),
  wxNotebook:addPage(Notebook, StatePanel, "State"),
  % wxNotebook:layout(Notebook),
  LeftSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],
  wxSizer:add(LeftSizer, Notebook, SizerFlags),
  LeftSizer.

setupCodePanel(Parent) ->
  CodePanel = wxPanel:new(Parent),
  CodeText = wxTextCtrl:new(CodePanel, ?CODE_TEXT,
                             [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  ref_add(?CODE_TEXT,CodeText),

  CodeSizer = wxBoxSizer:new(?wxVERTICAL),
  InputSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ref_add(?INPUT_SIZER, InputSizer),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(CodeSizer, CodeText, SizerFlags),
  wxSizer:addSpacer(CodeSizer, 10),
  wxSizer:add(CodeSizer, InputSizer, [{proportion, 0}, {flag, ?wxEXPAND}]),

  wxSizer:add(BorderSizer, CodeSizer, [{flag, ?wxALL bor ?wxEXPAND},
                                       {proportion, 1}, {border, 10}]),
  wxWindow:setSizer(CodePanel, BorderSizer),
  CodePanel.

 setupStatePanel(Parent) ->
  StatePanel = wxPanel:new(Parent),
  StateText = wxTextCtrl:new(StatePanel, ?STATE_TEXT,
                             [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  ref_add(?STATE_TEXT, StateText),
  StateSizer = wxBoxSizer:new(?wxVERTICAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],
  wxSizer:add(StateSizer, StateText, SizerFlags),
  wxSizer:add(BorderSizer, StateSizer, [{flag, ?wxALL bor ?wxEXPAND},
                                        {proportion, 1}, {border, 10}]),
  wxWindow:setSizer(StatePanel, BorderSizer),
  StatePanel.

setupTracePanel(Parent) ->
  TracePanel = wxPanel:new(Parent),
  TraceText = wxTextCtrl:new(TracePanel, ?TRACE_TEXT,
                             [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  ref_add(?TRACE_TEXT, TraceText),
  TraceSizer = wxBoxSizer:new(?wxVERTICAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],
  wxSizer:add(TraceSizer, TraceText, SizerFlags),
  wxSizer:add(BorderSizer, TraceSizer, [{flag, ?wxALL bor ?wxEXPAND},
                                        {proportion, 1}, {border, 10}]),
  wxWindow:setSizer(TracePanel, BorderSizer),
  TracePanel.

setupRollLogPanel(Parent) ->
  RollLogPanel = wxPanel:new(Parent),
  RollLogText = wxTextCtrl:new(RollLogPanel, ?ROLL_LOG_TEXT,
                               [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  ref_add(?ROLL_LOG_TEXT, RollLogText),
  RollLogSizer = wxBoxSizer:new(?wxVERTICAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],
  wxSizer:add(RollLogSizer, RollLogText, SizerFlags),
  wxSizer:add(BorderSizer, RollLogSizer, [{flag, ?wxALL bor ?wxEXPAND},
                                       {proportion, 1}, {border, 10}]),
  wxWindow:setSizer(RollLogPanel, BorderSizer),
  RollLogPanel.

setupRightSizer(Parent) ->
  Notebook = wxNotebook:new(Parent, ?RIGHT_NOTEBOOK),
  BottomNotebook = wxNotebook:new(Parent, ?RBOT_NOTEBOOK),
  ref_add(?RIGHT_NOTEBOOK, Notebook),
  ref_add(?RBOT_NOTEBOOK, BottomNotebook),
  ManuPanel = setupManualPanel(Notebook),
  ReplayPanel = setupReplayPanel(Notebook),
  RollPanel = setupRollPanel(Notebook),
  wxNotebook:addPage(Notebook, ManuPanel, "Manual"),
  wxNotebook:addPage(Notebook, ReplayPanel, "Replay"),
  wxNotebook:addPage(Notebook, RollPanel, "Rollback"),
  TracePanel = setupTracePanel(BottomNotebook),
  RollLogPanel = setupRollLogPanel(BottomNotebook),
  wxNotebook:addPage(BottomNotebook, TracePanel, "Trace"),
  wxNotebook:addPage(BottomNotebook, RollLogPanel, "Roll Log"),
  RightSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 0}, {flag, ?wxEXPAND}],
  BottomSizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],
  wxSizer:add(RightSizer, Notebook, SizerFlags),
  wxSizer:add(RightSizer, BottomNotebook, BottomSizerFlags),
  RightSizer.

setupManualPanel(Parent) ->
  ManuPanel = wxPanel:new(Parent),
  PidStaticText = wxStaticText:new(ManuPanel, ?wxID_ANY, "Pid"),
  PidTextCtrl = wxTextCtrl:new(ManuPanel, ?PID_TEXT, [{style, ?wxBOTTOM}]),
  ref_add(?PID_TEXT, PidTextCtrl),

  ForwIntButton = wxButton:new(ManuPanel, ?FORW_INT_BUTTON,
                               [{label, "Seq"}]),
  BackIntButton = wxButton:new(ManuPanel, ?BACK_INT_BUTTON,
                               [{label, "Seq"}]),
  wxButton:disable(ForwIntButton),
  wxButton:disable(BackIntButton),
  ref_add(?FORW_INT_BUTTON, ForwIntButton),
  ref_add(?BACK_INT_BUTTON, BackIntButton),


  ManuSizer = wxBoxSizer:new(?wxVERTICAL),
  ProcSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ForwardSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, ManuPanel,
                                      [{label, "Forward rules"}]),
  BackwardSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, ManuPanel,
                                      [{label, "Backward rules"}]),
  ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(ManuSizer, ProcSizer),
  wxSizer:addSpacer(ManuSizer, 10),
  wxSizer:add(ManuSizer, ButtonSizer),

  wxSizer:add(ProcSizer, PidStaticText, [{flag, ?wxCENTRE}]),
  wxSizer:add(ProcSizer, PidTextCtrl, [{flag, ?wxCENTRE}]),
  
  wxSizer:add(ForwardSizer, ForwIntButton),
  wxSizer:add(BackwardSizer, BackIntButton),

  wxSizer:add(ButtonSizer, ForwardSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:addSpacer(ButtonSizer, 5),
  wxSizer:add(ButtonSizer, BackwardSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

  wxSizer:add(BorderSizer, ManuSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(ManuPanel, BorderSizer),
  ManuPanel.

setupReplayPanel(Parent) ->
  ReplayPanel = wxPanel:new(Parent),
  ReplayPidStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "Pid:"),
  ReplayStepStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "Steps:"),
  ReplaySpawnIdStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "Pid:"),
  ReplaySendIdStaticText  = wxStaticText:new(ReplayPanel, ?wxID_ANY, "MsgId:"),
  ReplayRecIdStaticText   = wxStaticText:new(ReplayPanel, ?wxID_ANY, "MsgId:"),

  ReplayPidTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_PID_TEXT, [{style,?wxBOTTOM},
                                                               {size, {40, -1}}]),
  ReplayStepTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_STEP_TEXT, [{style,?wxBOTTOM},
                                                                 {size, {40, -1}}]),
  ReplaySpawnIdText = wxTextCtrl:new(ReplayPanel, ?REPLAY_SPAWN_ID_TEXT, [{style, ?wxBOTTOM},
                                                                    {size, {40, -1}}]),
  ReplaySendIdText  = wxTextCtrl:new(ReplayPanel, ?REPLAY_SEND_ID_TEXT, [{style, ?wxBOTTOM},
                                                                  {size, {40, -1}}]),
  ReplayRecIdText   = wxTextCtrl:new(ReplayPanel, ?REPLAY_REC_ID_TEXT, [{style, ?wxBOTTOM},
                                                                {size, {40, -1}}]),

  ref_add(?REPLAY_PID_TEXT, ReplayPidTextCtrl),
  ref_add(?REPLAY_STEP_TEXT, ReplayStepTextCtrl),
  ref_add(?REPLAY_SPAWN_ID_TEXT, ReplaySpawnIdText),
  ref_add(?REPLAY_SEND_ID_TEXT, ReplaySendIdText),
  ref_add(?REPLAY_REC_ID_TEXT, ReplayRecIdText),

  ReplayButton = wxButton:new(ReplayPanel, ?REPLAY_BUTTON,
                                [{label, "Replay"},
                                 {size, {100, -1}}]),
  ReplaySpawnButton = wxButton:new(ReplayPanel, ?REPLAY_SPAWN_BUTTON,
                               [{label, "Replay spawn"},
                                {size, {100, -1}}]),
  ReplaySendButton = wxButton:new(ReplayPanel, ?REPLAY_SEND_BUTTON,
                               [{label, "Replay send"},
                                {size, {100, -1}}]),
  ReplayRecButton = wxButton:new(ReplayPanel, ?REPLAY_REC_BUTTON,
                               [{label, "Replay rec"},
                                {size, {100, -1}}]),
  wxButton:disable(ReplayButton),
  wxButton:disable(ReplaySpawnButton),
  wxButton:disable(ReplaySendButton),
  wxButton:disable(ReplayRecButton),
  ref_add(?REPLAY_BUTTON, ReplayButton),
  ref_add(?REPLAY_SPAWN_BUTTON, ReplaySpawnButton),
  ref_add(?REPLAY_SEND_BUTTON, ReplaySendButton),
  ref_add(?REPLAY_REC_BUTTON, ReplayRecButton),

  ReplaySizer = wxBoxSizer:new(?wxVERTICAL),
  ReplayNSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ReplaySpawnSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ReplaySendSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ReplayRecSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(ReplayNSizer, ReplayPidStaticText),
  wxSizer:add(ReplayNSizer, ReplayPidTextCtrl),
  wxSizer:addSpacer(ReplayNSizer, 5),
  wxSizer:add(ReplayNSizer, ReplayStepStaticText),
  wxSizer:add(ReplayNSizer, ReplayStepTextCtrl),
  wxSizer:addSpacer(ReplayNSizer, 5),
  wxSizer:add(ReplayNSizer, ReplayButton),

  wxSizer:add(ReplaySizer, ReplayNSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(ReplaySizer, 10),
  wxSizer:add(ReplaySizer, ReplaySpawnSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(ReplaySizer, 10),
  wxSizer:add(ReplaySizer, ReplaySendSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(ReplaySizer, 10),
  wxSizer:add(ReplaySizer, ReplayRecSizer, [{flag, ?wxALIGN_RIGHT}]),

  wxSizer:add(ReplaySpawnSizer, ReplaySpawnIdStaticText),
  wxSizer:add(ReplaySpawnSizer, ReplaySpawnIdText),
  wxSizer:addSpacer(ReplaySpawnSizer, 5),
  wxSizer:add(ReplaySpawnSizer, ReplaySpawnButton),

  wxSizer:add(ReplaySendSizer, ReplaySendIdStaticText),
  wxSizer:add(ReplaySendSizer, ReplaySendIdText),
  wxSizer:addSpacer(ReplaySendSizer, 5),
  wxSizer:add(ReplaySendSizer, ReplaySendButton),

  wxSizer:add(ReplayRecSizer, ReplayRecIdStaticText),
  wxSizer:add(ReplayRecSizer, ReplayRecIdText),
  wxSizer:addSpacer(ReplayRecSizer, 5),
  wxSizer:add(ReplayRecSizer, ReplayRecButton),

  wxSizer:add(BorderSizer, ReplaySizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(ReplayPanel, BorderSizer),
  ReplayPanel.

setupRollPanel(Parent) ->
  RollPanel = wxPanel:new(Parent),
  RollPidStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Pid:"),
  RollStepStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Steps:"),
  RollSpawnIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Pid:"),
  RollSendIdStaticText  = wxStaticText:new(RollPanel, ?wxID_ANY, "MsgId:"),
  RollRecIdStaticText   = wxStaticText:new(RollPanel, ?wxID_ANY, "MsgId:"),
  RollVarIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Name:"),

  RollPidTextCtrl = wxTextCtrl:new(RollPanel, ?ROLL_PID_TEXT, [{style,?wxBOTTOM},
                                                               {size, {40, -1}}]),
  RollStepTextCtrl = wxTextCtrl:new(RollPanel, ?ROLL_STEP_TEXT, [{style,?wxBOTTOM},
                                                                 {size, {40, -1}}]),
  RollSpawnIdText = wxTextCtrl:new(RollPanel, ?ROLL_SPAWN_ID_TEXT, [{style, ?wxBOTTOM},
                                                                    {size, {40, -1}}]),
  RollSendIdText  = wxTextCtrl:new(RollPanel, ?ROLL_SEND_ID_TEXT, [{style, ?wxBOTTOM},
                                                                  {size, {40, -1}}]),
  RollRecIdText   = wxTextCtrl:new(RollPanel, ?ROLL_REC_ID_TEXT, [{style, ?wxBOTTOM},
                                                                {size, {40, -1}}]),
  RollVarIdText   = wxTextCtrl:new(RollPanel, ?ROLL_VAR_ID_TEXT, [{style, ?wxBOTTOM},
                                                                  {size, {80, -1}}]),

  ref_add(?ROLL_PID_TEXT, RollPidTextCtrl),
  ref_add(?ROLL_STEP_TEXT, RollStepTextCtrl),
  ref_add(?ROLL_SPAWN_ID_TEXT, RollSpawnIdText),
  ref_add(?ROLL_SEND_ID_TEXT, RollSendIdText),
  ref_add(?ROLL_REC_ID_TEXT, RollRecIdText),
  ref_add(?ROLL_VAR_ID_TEXT, RollVarIdText),

  RollButton = wxButton:new(RollPanel, ?ROLL_BUTTON,
                                [{label, "Roll"},
                                 {size, {85, -1}}]),
  RollSpawnButton = wxButton:new(RollPanel, ?ROLL_SPAWN_BUTTON,
                               [{label, "Roll spawn"},
                                {size, {85, -1}}]),
  RollSendButton = wxButton:new(RollPanel, ?ROLL_SEND_BUTTON,
                               [{label, "Roll send"},
                                {size, {85, -1}}]),
  RollRecButton = wxButton:new(RollPanel, ?ROLL_REC_BUTTON,
                               [{label, "Roll rec"},
                                {size, {85, -1}}]),
  RollVarButton = wxButton:new(RollPanel, ?ROLL_VAR_BUTTON,
                               [{label, "Roll var"},
                                {size, {85, -1}}]),
  wxButton:disable(RollButton),
  wxButton:disable(RollSpawnButton),
  wxButton:disable(RollSendButton),
  wxButton:disable(RollRecButton),
  wxButton:disable(RollVarButton),
  ref_add(?ROLL_BUTTON, RollButton),
  ref_add(?ROLL_SPAWN_BUTTON, RollSpawnButton),
  ref_add(?ROLL_SEND_BUTTON, RollSendButton),
  ref_add(?ROLL_REC_BUTTON, RollRecButton),
  ref_add(?ROLL_VAR_BUTTON, RollVarButton),

  RollSizer = wxBoxSizer:new(?wxVERTICAL),
  RollNSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollSpawnSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollSendSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollRecSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollVarSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(RollNSizer, RollPidStaticText),
  wxSizer:add(RollNSizer, RollPidTextCtrl),
  wxSizer:addSpacer(RollNSizer, 5),
  wxSizer:add(RollNSizer, RollStepStaticText),
  wxSizer:add(RollNSizer, RollStepTextCtrl),
  wxSizer:addSpacer(RollNSizer, 5),
  wxSizer:add(RollNSizer, RollButton),

  wxSizer:add(RollSizer, RollNSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollSpawnSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollSendSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollRecSizer, [{flag, ?wxALIGN_RIGHT}]),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollVarSizer, [{flag, ?wxALIGN_RIGHT}]),

  wxSizer:add(RollSpawnSizer, RollSpawnIdStaticText),
  wxSizer:add(RollSpawnSizer, RollSpawnIdText),
  wxSizer:addSpacer(RollSpawnSizer, 5),
  wxSizer:add(RollSpawnSizer, RollSpawnButton),

  wxSizer:add(RollSendSizer, RollSendIdStaticText),
  wxSizer:add(RollSendSizer, RollSendIdText),
  wxSizer:addSpacer(RollSendSizer, 5),
  wxSizer:add(RollSendSizer, RollSendButton),

  wxSizer:add(RollRecSizer, RollRecIdStaticText),
  wxSizer:add(RollRecSizer, RollRecIdText),
  wxSizer:addSpacer(RollRecSizer, 5),
  wxSizer:add(RollRecSizer, RollRecButton),

  wxSizer:add(RollVarSizer, RollVarIdStaticText),
  wxSizer:add(RollVarSizer, RollVarIdText),
  wxSizer:addSpacer(RollVarSizer, 5),
  wxSizer:add(RollVarSizer, RollVarButton),

  wxSizer:add(BorderSizer, RollSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(RollPanel, BorderSizer),
  RollPanel.

setupMenu() ->
  MenuBar = wxMenuBar:new(),
  File = wxMenu:new(),
  View = wxMenu:new(),
  Compile = wxMenu:new(),
  Sched  = wxMenu:new(),
  Help = wxMenu:new(),
  ref_add(?MENU_VIEW, View),
  ref_add(?MENU_COMP, Compile),
  ref_add(?MENU_SCHED, Sched),
  wxMenuBar:append(MenuBar, File, "&File"),
  wxMenuBar:append(MenuBar, View, "&View"),
  wxMenuBar:append(MenuBar, Compile, "&Compiler"),
  wxMenuBar:append(MenuBar, Help, "&Help"),
  OpenItem = wxMenu:append(File, ?OPEN,     "Open\tCtrl-O"),
  ReplayItem = wxMenu:append(File, ?REPLAY, "Load Trace\tCtrl-T"),
  QuitItem = wxMenu:append(File, ?EXIT,     "Quit\tCtrl-Q"),
  ZoomInItem = wxMenu:append(View, ?ZOOM_IN,  "Zoom In\tCtrl-+"),
  ZoomOutItem = wxMenu:append(View, ?ZOOM_OUT, "Zoom Out\tCtrl--"),
  wxMenu:appendSeparator(View),
  ToggleMail = wxMenu:appendCheckItem(View, ?TOGGLE_MAIL, "Toggle Mailboxes"),
  ToggleHist = wxMenu:appendCheckItem(View, ?TOGGLE_HIST, "Toggle Histories"),
  ToggleEnv  = wxMenu:appendCheckItem(View, ?TOGGLE_ENV,  "Toggle Environments"),
  ToggleExp  = wxMenu:appendCheckItem(View, ?TOGGLE_EXP,  "Toggle Expressions"),
  wxMenu:appendSeparator(View),
  RadioConc  = wxMenu:appendRadioItem(View, ?RADIO_CONC, "Conc. History"),
  RadioFull = wxMenu:appendRadioItem(View, ?RADIO_FULL, "Full History"),
  wxMenu:appendSeparator(View),
  RadioRelEnv   = wxMenu:appendRadioItem(View, ?RADIO_REL_ENV, "Relevant Environment"),
  RadioFullEnv = wxMenu:appendRadioItem(View, ?RADIO_FULL_ENV, "Full Environment"),
  wxMenu:appendSeparator(View),
  wxMenuItem:check(ToggleMail),
  wxMenuItem:check(ToggleHist),
  wxMenuItem:check(ToggleEnv),
  wxMenuItem:check(ToggleExp),
  wxMenuItem:check(RadioConc),
  wxMenuItem:check(RadioRelEnv),
  ToggleComp  = wxMenu:appendCheckItem(Compile, ?TOGGLE_COMP,  "Compiler Optimizations"),
  wxMenuItem:check(ToggleComp),
  wxMenu:append(Help, ?ABOUT, "About"),
  wxMenuItem:setHelp(OpenItem,     ?HELP_OPEN_ITEM),
  wxMenuItem:setHelp(ReplayItem,   ?HELP_REPLAY_ITEM),
  wxMenuItem:setHelp(QuitItem,     ?HELP_QUIT_ITEM),
  wxMenuItem:setHelp(ZoomInItem,   ?HELP_ZOOM_IN_ITEM),
  wxMenuItem:setHelp(ZoomOutItem,  ?HELP_ZOOM_OUT_ITEM),
  wxMenuItem:setHelp(ToggleMail,   ?HELP_TOGGLE_MAIL),
  wxMenuItem:setHelp(ToggleHist,   ?HELP_TOGGLE_HIST),
  wxMenuItem:setHelp(ToggleEnv,    ?HELP_TOGGLE_ENV),
  wxMenuItem:setHelp(ToggleExp,    ?HELP_TOGGLE_EXP),
  wxMenuItem:setHelp(RadioConc,    ?HELP_RADIO_CONC),
  wxMenuItem:setHelp(RadioFull,    ?HELP_RADIO_FULL),
  wxMenuItem:setHelp(RadioRelEnv,  ?HELP_RADIO_REN_ENV),
  wxMenuItem:setHelp(RadioFullEnv, ?HELP_RADIO_FULL_ENV),
  wxMenuItem:setHelp(ToggleComp,   ?HELP_TOGGLE_COMP),
  Frame = ref_lookup(?FRAME),
  wxFrame:setMenuBar(Frame, MenuBar).

loadFile(File) ->
  Frame = ref_lookup(?FRAME),
  ToggleOpts = utils_gui:toggle_opts(),
  AddOptimize = proplists:get_value(?COMP_OPT, ToggleOpts),
  CompOpts =
    case AddOptimize of
      true  -> [to_core,binary];
      false -> [to_core,binary, no_copt]
    end,
  case compile:file(File, CompOpts) of
    {ok, _, CoreForms} ->
      NoAttsCoreForms = cerl:update_c_module(CoreForms,
                                             cerl:module_name(CoreForms),
                                             cerl:module_exports(CoreForms),
                                             [],
                                             cerl:module_defs(CoreForms)),
      Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
      CleanCoreForms = cerl_trees:map(Stripper, NoAttsCoreForms),
      FunDefs = cerl:module_defs(CleanCoreForms),
      CodeText = ref_lookup(?CODE_TEXT),
      wxTextCtrl:setValue(CodeText, core_pp:format(CleanCoreForms)),
      Status = ref_lookup(?STATUS),
      ref_add(?STATUS, Status#status{loaded = {true, FunDefs}}),
      LeftNotebook = ref_lookup(?LEFT_NOTEBOOK),
      wxNotebook:setSelection(LeftNotebook, ?PAGEPOS_CODE),
      % utils_gui:set_choices(utils:moduleNames(CleanCoreForms)),
      utils_gui:disable_all_buttons(),
      utils_gui:clear_texts(),
      % InputSizer = ref_lookup(?INPUT_SIZER),
      % wxSizer:layout(InputSizer),
      % StartButton = ref_lookup(?START_BUTTON),
      % wxButton:enable(StartButton),
      wxFrame:setStatusText(Frame, "Loaded file " ++ File);
    _Other ->
      wxFrame:setStatusText(Frame, "Error: Could not compile file " ++ File)
  end.

loadReplayData(Path) ->
  utils:extract_replay_data(Path),
  ReplayData = get(replay_data),
  {_Mod, Fun, Args} = utils:get_mod_name(ReplayData#replay.call),
  MainPid = ReplayData#replay.main_pid,
  MainLog = utils:extract_pid_log_data(Path, MainPid),
  SMainPid = utils:log_token_val(MainPid),
  start(cerl:c_var({Fun,length(Args)}), Args, SMainPid, MainLog).

openDialog(Parent) ->
  Caption = "Select an Erlang file",
  Wildcard = "Erlang source|*.erl| All files|*",
  DefaultDir = ref_lookup(?FILE_PATH),
  DefaultFile = "",
  Dialog = wxFileDialog:new(Parent, [{message, Caption},
                                     {defaultDir, DefaultDir},
                                     {defaultFile, DefaultFile},
                                     {wildCard, Wildcard},
                                     {style, ?wxFD_OPEN bor
                                          ?wxFD_FILE_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
      ?wxID_OK ->
        File = wxFileDialog:getPaths(Dialog),
        loadFile(File);
      _Other -> continue
  end,
  wxDialog:destroy(Dialog).

openReplayDialog(Parent) ->
  Caption = "Select a log folder",
  DefaultPath = ref_lookup(?FILE_PATH),
  Dialog = wxDirDialog:new(Parent, [{title, Caption},
                                    {defaultPath, DefaultPath},
                                    {style, ?wxDD_DIR_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
      ?wxID_OK ->
        Path = wxDirDialog:getPath(Dialog),
        loadReplayData(Path);
      _Other -> continue
  end,
  wxDialog:destroy(Dialog).

zoomIn() ->
  CodeText = ref_lookup(?CODE_TEXT),
  StateText = ref_lookup(?STATE_TEXT),
  Font = wxTextCtrl:getFont(CodeText),
  CurFontSize = wxFont:getPointSize(Font),
  NewFontSize = utils_gui:next_font_size(CurFontSize),
  NewFont = wxFont:new(),
  wxFont:setPointSize(NewFont, NewFontSize),
  wxTextCtrl:setFont(CodeText, NewFont),
  wxTextCtrl:setFont(StateText, NewFont).

zoomOut() ->
  CodeText = ref_lookup(?CODE_TEXT),
  StateText = ref_lookup(?STATE_TEXT),
  Font = wxTextCtrl:getFont(CodeText),
  CurFontSize = wxFont:getPointSize(Font),
  NewFontSize = utils_gui:prev_font_size(CurFontSize),
  NewFont = wxFont:new(),
  wxFont:setPointSize(NewFont, NewFontSize),
  wxTextCtrl:setFont(CodeText, NewFont),
  wxTextCtrl:setFont(StateText, NewFont).

load_ghosts(ExceptPid) ->
  ReplayData = get(replay_data),
  Path = ReplayData#replay.log_path,
  {ok, Filenames} = file:list_dir(Path),
  RestFilenames = Filenames -- ["trace_result.log",
                                "trace_" ++ integer_to_list(ExceptPid) ++ ".log"],
  Captures = [re:run(FileName, "trace_(\\d+).log", [{capture,[1]}]) || FileName <- RestFilenames],
  ZCaptures = lists:zip(Captures, RestFilenames),
  FCaptures = [{hd(L), F} || {{match, L}, F} <- ZCaptures],
  Pids = [string:substr(F, I + 1, L) || {{I,L},F} <- FCaptures],
  [begin
     #proc{pid = cerl:c_int(list_to_integer(P)),
           log = utils:extract_pid_log_data(Path, P)}
   end ||
     P <- Pids].

init_system(Fun, Args, Pid, Log) ->
  Proc = #proc{pid = cerl:c_int(Pid),
               log = Log,
               exp = cerl:c_apply(Fun, Args),
               spf = cerl:var_name(Fun)},
  GhostProcs = load_ghosts(Pid),
  Procs = [Proc],
  System = #sys{procs = Procs, ghosts = GhostProcs},
  ref_add(?SYSTEM, System),
  Status = ref_lookup(?STATUS),
  NewStatus = Status#status{running = true},
  ref_add(?STATUS, NewStatus).

% start(Fun,Args) ->
%   start(Fun, Args, 1, []).

start(Fun,Args, Pid, Log) ->
  Status = ref_lookup(?STATUS),
  #status{loaded = {true, FunDefs}} = Status,
  utils_gui:stop_refs(),
  cauder:start_refs(FunDefs),
  init_system(Fun, Args, Pid, Log),
  refresh(true),
  LeftNotebook = ref_lookup(?LEFT_NOTEBOOK),
  wxNotebook:setSelection(LeftNotebook, ?PAGEPOS_STATE),
  {FunName, FunArity} = cerl:var_name(Fun),
  StartString = "Started system with " ++
                atom_to_list(FunName) ++ "/" ++
                integer_to_list(FunArity) ++ " fun application!",
  utils_gui:update_status_text(StartString).

refresh_buttons(Options) ->
  PidTextCtrl = ref_lookup(?PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  ManualButtons = lists:seq(?FORW_INT_BUTTON, ?BACK_INT_BUTTON),
  ?LOG("full options: " ++ ?TO_STRING(utils_gui:sort_opts(Options))),
  case string:to_integer(PidText) of
    {error, _} ->
      utils_gui:disable_rule_buttons(ManualButtons);
    {PidInt, _} ->
      FiltOpts = utils:filter_options(Options, PidInt),
      FiltButtons = lists:map(fun utils_gui:option_to_button_label/1, FiltOpts),
      [utils_gui:set_button_label_if(Button, FiltButtons) ||
                               Button <- ManualButtons]
  end.
  % HasFwdOptions = utils:has_fwd(Options),
  % HasBwdOptions = utils:has_bwd(Options),
  % HasNormOptions = utils:has_norm(Options),
  % utils_gui:set_ref_button_if(?FORWARD_BUTTON, HasFwdOptions),
  % utils_gui:set_ref_button_if(?BACKWARD_BUTTON, HasBwdOptions),
  % utils_gui:set_ref_button_if(?NORMALIZE_BUTTON, HasNormOptions).

refresh(RefState) ->
  case utils_gui:is_app_running() of
    false -> ok;
    true ->
      System = ref_lookup(?SYSTEM),
      Options = cauder:eval_opts(System),
      case RefState of
        false -> ok;
        true  ->
          ToggleOpts = utils_gui:toggle_opts(),
          StateText = ref_lookup(?STATE_TEXT),
          TraceText = ref_lookup(?TRACE_TEXT),
          RollLogText = ref_lookup(?ROLL_LOG_TEXT),
          MarkedText = utils:pp_system(System, ToggleOpts),
          utils_gui:pp_marked_text(StateText, MarkedText),
          wxTextCtrl:setValue(TraceText,utils:pp_trace(System)),
          wxTextCtrl:setValue(RollLogText,utils:pp_roll_log(System))
      end,
      refresh_buttons(Options),
      utils_gui:enable_perm_buttons()
  end.

exec_with(Button) ->
  System = ref_lookup(?SYSTEM),
  PidTextCtrl = ref_lookup(?PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  case string:to_integer(PidText) of
    {error, _} ->
      ok;
    {PidInt, _} ->
      PartOption = utils_gui:button_to_option(Button),
      Option = PartOption#opt{id = PidInt},
      NewSystem = cauder:eval_step(System, Option),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_replay() ->
  System = ref_lookup(?SYSTEM),
  PidTextCtrl = ref_lookup(?REPLAY_PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  StepTextCtrl = ref_lookup(?REPLAY_STEP_TEXT),
  StepText = wxTextCtrl:getValue(StepTextCtrl),
  {Pid, _} = string:to_integer(PidText),
  {Steps, _} = string:to_integer(StepText),
  case {Pid, Steps} of
    {error, _} -> {false, 0, 0};
    {_, error} -> {false, 0, 0};
    _ ->
      CorePid = cerl:c_int(Pid),
      {NewSystem, StepsDone} = cauder:eval_replay(System, CorePid, Steps),
      ref_add(?SYSTEM, NewSystem),
      {StepsDone, Steps}
  end.

eval_roll() ->
  System = ref_lookup(?SYSTEM),
  PidTextCtrl = ref_lookup(?ROLL_PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  StepTextCtrl = ref_lookup(?ROLL_STEP_TEXT),
  StepText = wxTextCtrl:getValue(StepTextCtrl),
  {Pid, _} = string:to_integer(PidText),
  {Steps, _} = string:to_integer(StepText),
  case {Pid, Steps} of
    {error, _} -> {false, 0, 0};
    {_, error} -> {false, 0, 0};
    _ ->
      CorePid = cerl:c_int(Pid),
      {FocusLog, NewSystem, StepsDone} = cauder:eval_roll(System, CorePid, Steps),
      ref_add(?SYSTEM, NewSystem),
      {FocusLog, StepsDone, Steps}
  end.

eval_replay_spawn() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?REPLAY_SPAWN_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    % What if error?
    error -> {false, ok, false};
    _ ->
      NewSystem = cauder:eval_replay_spawn(System, cerl:c_int(Id)),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_replay_send() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?REPLAY_SEND_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    % What if error?
    error -> {false, ok, false};
    _ ->
      NewSystem = cauder:eval_replay_send(System, Id),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_replay_rec() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?REPLAY_REC_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    % What if error?
    error -> {false, ok, false};
    _ ->
      NewSystem = cauder:eval_replay_rec(System, Id),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_roll_send() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_SEND_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    error -> {false, ok, false};
    _ ->
      {CanRoll, FocusLog, NewSystem} = cauder:eval_roll_send(System, Id),
      ref_add(?SYSTEM, NewSystem),
      {CanRoll, IdText, FocusLog}
  end.

eval_roll_spawn() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_SPAWN_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    error -> {false, ok, false};
    _ ->
      {CanRoll, FocusLog, NewSystem} = cauder:eval_roll_spawn(System, cerl:c_int(Id)),
      ref_add(?SYSTEM, NewSystem),
      {CanRoll, IdText, FocusLog}
  end.

eval_roll_rec() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_REC_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    error -> {false, ok, false};
    _ ->
      {CanRoll, FocusLog, NewSystem} = cauder:eval_roll_rec(System, Id),
      ref_add(?SYSTEM, NewSystem),
      {CanRoll, IdText, FocusLog}
  end.

eval_roll_var() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_VAR_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  case IdText of
    "" -> {false, ok, false};
    _ ->
      % Variables such as '@c1_X' appear as '_@c1_X'
      % This case removes the "_" from the variable
      % name if it is the first character
      VarName =
        case string:find(IdText, "_") of
          no_match -> IdText;
          Match when length(IdText) =:= length(Match) ->
            string:slice(IdText, 1);
          _ ->
            IdText
        end,
      Var = cerl:c_var(list_to_atom(VarName)),
      {CanRoll, FocusLog, NewSystem} = cauder:eval_roll_var(System, Var),
      ref_add(?SYSTEM, NewSystem),
      {CanRoll, VarName, FocusLog}
  end.

focus_roll_log(false) -> ok;
focus_roll_log(true) ->
  RBotNotebook = ref_lookup(?RBOT_NOTEBOOK),
  wxNotebook:setSelection(RBotNotebook, ?PAGEPOS_ROLL).

loop() ->
    receive
        %% ------------------- Button handlers ------------------- %%
        #wx{id = ?ROLL_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          {MustFocus, StepsDone, TotalSteps} = eval_roll(),
          utils_gui:sttext_roll(StepsDone, TotalSteps),
          focus_roll_log(MustFocus),
          refresh(true),
          loop();
        #wx{id = ?REPLAY_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          eval_replay(),
          % utils_gui:sttext_roll(StepsDone, TotalSteps),
          refresh(true),
          loop();
        #wx{id = RuleButton, event = #wxCommand{type = command_button_clicked}}
          when (RuleButton >= ?FORW_INT_BUTTON) and (RuleButton =< ?BACK_INT_BUTTON) ->
          utils_gui:disable_all_buttons(),
          exec_with(RuleButton),
          utils_gui:sttext_single(RuleButton),
          refresh(true),
          loop();
        #wx{id = ?REPLAY_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          eval_replay_spawn(),
          refresh(true),
          loop();
        #wx{id = ?REPLAY_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          eval_replay_send(),
          refresh(true),
          loop();
        #wx{id = ?REPLAY_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          eval_replay_rec(),
          refresh(true),
          loop();
        #wx{id = ?ROLL_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          {HasRolled, SendId, MustFocus} = eval_roll_send(),
          utils_gui:sttext_roll_send(HasRolled, SendId),
          focus_roll_log(MustFocus),
          refresh(HasRolled),
          loop();
        #wx{id = ?ROLL_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          {HasRolled, SpawnId, MustFocus} = eval_roll_spawn(),
          utils_gui:sttext_roll_spawn(HasRolled, SpawnId),
          focus_roll_log(MustFocus),
          refresh(HasRolled),
          loop();
        #wx{id = ?ROLL_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          {HasRolled, RecId, MustFocus} = eval_roll_rec(),
          utils_gui:sttext_roll_rec(HasRolled, RecId),
          focus_roll_log(MustFocus),
          refresh(HasRolled),
          loop();
        #wx{id = ?ROLL_VAR_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          utils_gui:disable_all_buttons(),
          {HasRolled, VarId, MustFocus} = eval_roll_var(),
          utils_gui:sttext_roll_var(HasRolled, VarId),
          focus_roll_log(MustFocus),
          refresh(HasRolled),
          loop();
        %% -------------------- Text handlers -------------------- %%
        #wx{id = ?PID_TEXT, event = #wxCommand{type = command_text_updated}} ->
          refresh(false),
          loop();
        #wx{id = ?STEP_TEXT, event = #wxCommand{type = command_text_updated}} ->
          refresh(false),
          loop();
        #wx{id = _RestIds, event = #wxCommand{type = command_text_updated}} ->
          loop();
        %% -------------------- Menu handlers -------------------- %%
        #wx{id = ?ABOUT, event = #wxCommand{type = command_menu_selected}} ->
          Caption = "About " ++ ?APP_STRING,
          Frame = ref_lookup(?FRAME),
          Dialog = wxMessageDialog:new(Frame, ?INFO_TEXT,
                                       [{style, ?wxOK},
                                        {caption, Caption}]),
          wxDialog:showModal(Dialog),
          wxWindow:destroy(Dialog),
          loop();
        #wx{id = ?OPEN, event = #wxCommand{type = command_menu_selected}} ->
          Frame = ref_lookup(?FRAME),
          openDialog(Frame),
          loop();
        #wx{id = ?REPLAY, event = #wxCommand{type = command_menu_selected}} ->
          Frame = ref_lookup(?FRAME),
          openReplayDialog(Frame),
          loop();
        #wx{id = ?ZOOM_IN, event = #wxCommand{type = command_menu_selected}} ->
          zoomIn(),
          loop();
        #wx{id = ?ZOOM_OUT, event = #wxCommand{type = command_menu_selected}} ->
          zoomOut(),
          loop();
        #wx{id = ?TOGGLE_MAIL, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?TOGGLE_HIST, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?TOGGLE_ENV, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?TOGGLE_EXP, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?RADIO_CONC, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?RADIO_FULL, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?RADIO_REL_ENV, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?RADIO_FULL_ENV, event = #wxCommand{type = command_menu_selected}} ->
          refresh(true),
          loop();
        #wx{id = ?TOGGLE_COMP, event = #wxCommand{type = command_menu_selected}} ->
          utils_gui:sttext_comp(),
          loop();
        #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} ->
          Frame = ref_lookup(?FRAME),
          wxFrame:destroy(Frame);
        %% ------------------- Other handlers -------------------- %%
        #wx{event = #wxClose{type = close_window}} ->
          Frame = ref_lookup(?FRAME),
          wxFrame:destroy(Frame);
        %% ---------------- Non-supported events ----------------- %%
        Other ->
          io:format("main loop does not implement ~p~n", [Other]),
          loop()
    end.

ref_add(Id, Ref) ->
    ets:insert(?GUI_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?GUI_REF, Id, 2).

ref_start() ->
    ?GUI_REF = ets:new(?GUI_REF, [set, public, named_table]),
    ok.

ref_stop() ->
    ets:delete(?GUI_REF).
