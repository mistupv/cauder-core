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


  FundefStaticText = wxStaticText:new(CodePanel, ?wxID_ANY, "Funs: "),
  FunChoice = wxChoice:new(CodePanel, ?wxID_ANY),
  ref_add(?FUN_CHOICE,FunChoice),
  InputStaticText = wxStaticText:new(CodePanel, ?wxID_ANY, "Input args: "),
  InputTextCtrl = wxTextCtrl:new(CodePanel, ?INPUT_TEXT,
                                 [{style, ?wxBOTTOM},
                                  {value, ""}]),
  ref_add(?INPUT_TEXT,InputTextCtrl),
  StartButton = wxButton:new(CodePanel, ?START_BUTTON,
                             [{label, "START"}]),
  ref_add(?START_BUTTON,StartButton),
  wxButton:disable(StartButton),

  CodeSizer = wxBoxSizer:new(?wxVERTICAL),
  InputSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ref_add(?INPUT_SIZER, InputSizer),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(CodeSizer, CodeText, SizerFlags),
  wxSizer:addSpacer(CodeSizer, 10),
  wxSizer:add(CodeSizer, InputSizer, [{proportion, 0}, {flag, ?wxEXPAND}]),

  wxSizer:add(InputSizer, FundefStaticText),
  wxSizer:add(InputSizer, FunChoice),
  wxSizer:addSpacer(InputSizer, 10),
  wxSizer:add(InputSizer, InputStaticText),
  wxSizer:add(InputSizer, InputTextCtrl, SizerFlags),
  wxSizer:addSpacer(InputSizer, 10),
  wxSizer:add(InputSizer, StartButton, [{flag, ?wxALIGN_RIGHT}]),

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
  AutoPanel = setupAutoPanel(Notebook),
  RollPanel = setupRollPanel(Notebook),
  wxNotebook:addPage(Notebook, ManuPanel, "Manual"),
  wxNotebook:addPage(Notebook, AutoPanel, "Automatic"),
  wxNotebook:addPage(Notebook, RollPanel, "Rollback"),
  % wxNotebook:layout(Notebook),
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
  PidStaticText = wxStaticText:new(ManuPanel, ?wxID_ANY, "Pid/MsgId:"),
  PidTextCtrl = wxTextCtrl:new(ManuPanel, ?PID_TEXT, [{style, ?wxBOTTOM}]),
  ref_add(?PID_TEXT, PidTextCtrl),

  ForwIntButton = wxButton:new(ManuPanel, ?FORW_INT_BUTTON,
                               [{label, "Seq"}]),
  ForwSchButton = wxButton:new(ManuPanel, ?FORW_SCH_BUTTON,
                                [{label, "Sched"}]),
  BackIntButton = wxButton:new(ManuPanel, ?BACK_INT_BUTTON,
                               [{label, "Seq"}]),
  BackSchButton = wxButton:new(ManuPanel, ?BACK_SCH_BUTTON,
                                [{label, "Sched"}]),
  wxButton:disable(ForwIntButton),
  wxButton:disable(ForwSchButton),
  wxButton:disable(BackIntButton),
  wxButton:disable(BackSchButton),
  ref_add(?FORW_INT_BUTTON, ForwIntButton),
  ref_add(?FORW_SCH_BUTTON, ForwSchButton),
  ref_add(?BACK_INT_BUTTON, BackIntButton),
  ref_add(?BACK_SCH_BUTTON, BackSchButton),

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
  wxSizer:addSpacer(ForwardSizer, 5),
  wxSizer:add(ForwardSizer, ForwSchButton),
  wxSizer:add(BackwardSizer, BackIntButton),
  wxSizer:addSpacer(BackwardSizer, 5),
  wxSizer:add(BackwardSizer, BackSchButton),

  wxSizer:add(ButtonSizer, ForwardSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:addSpacer(ButtonSizer, 5),
  wxSizer:add(ButtonSizer, BackwardSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

  wxSizer:add(BorderSizer, ManuSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(ManuPanel, BorderSizer),
  ManuPanel.

setupAutoPanel(Parent) ->
  AutoPanel = wxPanel:new(Parent),
  StepStaticText = wxStaticText:new(AutoPanel, ?wxID_ANY, "Steps:"),
  RollPidStaticText = wxStaticText:new(AutoPanel, ?wxID_ANY, "Pid:"),
  RollStepStaticText = wxStaticText:new(AutoPanel, ?wxID_ANY, "Steps:"),
  StepTextCtrl = wxTextCtrl:new(AutoPanel, ?STEP_TEXT, [{style,?wxBOTTOM}]),
  RollPidTextCtrl = wxTextCtrl:new(AutoPanel, ?ROLL_PID_TEXT, [{style,?wxBOTTOM},
                                                               {size, {40, -1}}]),
  RollStepTextCtrl = wxTextCtrl:new(AutoPanel, ?ROLL_STEP_TEXT, [{style,?wxBOTTOM},
                                                                 {size, {40, -1}}]),
  ref_add(?STEP_TEXT, StepTextCtrl),
  ref_add(?ROLL_PID_TEXT, RollPidTextCtrl),
  ref_add(?ROLL_STEP_TEXT, RollStepTextCtrl),
  HorizontalLine = wxStaticLine:new(AutoPanel, [{style, ?wxLI_HORIZONTAL},
                                                {size, {200, -1}}]),
  HorizontalLine2 = wxStaticLine:new(AutoPanel, [{style, ?wxLI_HORIZONTAL},
                                                {size, {200, -1}}]),
  ForwardButton = wxButton:new(AutoPanel, ?FORWARD_BUTTON,
                               [{label, "Forward"}]),
  BackwardButton = wxButton:new(AutoPanel, ?BACKWARD_BUTTON,
                                [{label, "Backward"}]),
  NormalizeButton = wxButton:new(AutoPanel, ?NORMALIZE_BUTTON,
                                [{label, "Normalize"}]),
  RollButton = wxButton:new(AutoPanel, ?ROLL_BUTTON,
                                [{label, "Roll"},
                                 {size, {40, -1}}]),
  wxButton:disable(ForwardButton),
  wxButton:disable(BackwardButton),
  wxButton:disable(NormalizeButton),
  %wxButton:disable(RollButton),
  ref_add(?FORWARD_BUTTON, ForwardButton),
  ref_add(?BACKWARD_BUTTON, BackwardButton),
  ref_add(?NORMALIZE_BUTTON, NormalizeButton),
  ref_add(?ROLL_BUTTON, RollButton),

  AutoSizer = wxBoxSizer:new(?wxVERTICAL),
  StepSizer = wxBoxSizer:new(?wxHORIZONTAL),
  StepButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  SchedButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(AutoSizer, StepSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:addSpacer(AutoSizer, 15),
  wxSizer:add(AutoSizer, StepButtonSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:add(AutoSizer, HorizontalLine, [{flag, ?wxTOP bor ?wxBOTTOM},
                                          {border, 15}]),
  wxSizer:add(AutoSizer, SchedButtonSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:add(AutoSizer, HorizontalLine2, [{flag, ?wxTOP bor ?wxBOTTOM},
                                          {border, 15}]),
  wxSizer:add(AutoSizer, RollSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

  wxSizer:add(StepSizer, StepStaticText),
  wxSizer:add(StepSizer, StepTextCtrl),

  wxSizer:add(StepButtonSizer, ForwardButton),
  wxSizer:addSpacer(StepButtonSizer, 5),
  wxSizer:add(StepButtonSizer, BackwardButton),

  wxSizer:add(SchedButtonSizer, NormalizeButton),

  wxSizer:add(RollSizer, RollPidStaticText),
  wxSizer:add(RollSizer, RollPidTextCtrl),
  wxSizer:addSpacer(RollSizer, 5),
  wxSizer:add(RollSizer, RollStepStaticText),
  wxSizer:add(RollSizer, RollStepTextCtrl),
  wxSizer:addSpacer(RollSizer, 5),
  wxSizer:add(RollSizer, RollButton),

  wxSizer:add(BorderSizer, AutoSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(AutoPanel, BorderSizer),
  AutoPanel.

setupRollPanel(Parent) ->
  RollPanel = wxPanel:new(Parent),
  RollSpawnIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Pid:"),
  RollSendIdStaticText  = wxStaticText:new(RollPanel, ?wxID_ANY, "MsgId:"),
  RollRecIdStaticText   = wxStaticText:new(RollPanel, ?wxID_ANY, "MsgId:"),
  RollVarIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Name:"),
  RollSpawnIdText = wxTextCtrl:new(RollPanel, ?ROLL_SPAWN_ID_TEXT, [{style, ?wxBOTTOM},
                                                                    {size, {40, -1}}]),
  RollSendIdText  = wxTextCtrl:new(RollPanel, ?ROLL_SEND_ID_TEXT, [{style, ?wxBOTTOM},
                                                                  {size, {40, -1}}]),
  RollRecIdText   = wxTextCtrl:new(RollPanel, ?ROLL_REC_ID_TEXT, [{style, ?wxBOTTOM},
                                                                {size, {40, -1}}]),
  RollVarIdText   = wxTextCtrl:new(RollPanel, ?ROLL_VAR_ID_TEXT, [{style, ?wxBOTTOM},
                                                                  {size, {80, -1}}]),
                                                                %{size, {40, -1}}]),
  ref_add(?ROLL_SPAWN_ID_TEXT, RollSpawnIdText),
  ref_add(?ROLL_SEND_ID_TEXT, RollSendIdText),
  ref_add(?ROLL_REC_ID_TEXT, RollRecIdText),
  ref_add(?ROLL_VAR_ID_TEXT, RollVarIdText),

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

  ref_add(?ROLL_SPAWN_BUTTON, RollSpawnButton),
  ref_add(?ROLL_SEND_BUTTON, RollSendButton),
  ref_add(?ROLL_REC_BUTTON, RollRecButton),
  ref_add(?ROLL_VAR_BUTTON, RollVarButton),

  RollSizer = wxBoxSizer:new(?wxVERTICAL),
  RollSpawnSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollSendSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollRecSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollVarSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

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
  Help = wxMenu:new(),
  ref_add(?MENU_VIEW, View),
  wxMenuBar:append(MenuBar, File, "&File"),
  wxMenuBar:append(MenuBar, View, "&View"),
  wxMenuBar:append(MenuBar, Help, "&Help"),
  wxMenu:append(File, ?OPEN,     "Open\tCtrl-O"),
  wxMenu:append(File, ?EXIT,     "Quit\tCtrl-Q"),
  wxMenu:append(View, ?ZOOM_IN,  "Zoom In\tCtrl-+"),
  wxMenu:append(View, ?ZOOM_OUT, "Zoom Out\tCtrl--"),
  wxMenu:appendSeparator(View),
  ToggleMail = wxMenu:appendCheckItem(View, ?TOGGLE_MAIL, "Toggle Mailboxes \tCtrl-M"),
  ToggleHist = wxMenu:appendCheckItem(View, ?TOGGLE_HIST, "Toggle Histories \tCtrl-H"),
  ToggleEnv  = wxMenu:appendCheckItem(View, ?TOGGLE_ENV,  "Toggle Environments \tCtrl-T"),
  ToggleExp  = wxMenu:appendCheckItem(View, ?TOGGLE_EXP,  "Toggle Expressions \tCtrl-E"),
  wxMenuItem:check(ToggleMail),
  wxMenuItem:check(ToggleHist),
  wxMenuItem:check(ToggleEnv),
  wxMenuItem:check(ToggleExp),
  wxMenu:appendSeparator(View),
  wxMenu:append(Help, ?ABOUT,    "About"),
  Frame = ref_lookup(?FRAME),
  wxFrame:setMenuBar(Frame, MenuBar).

loadFile(File) ->
  Frame = ref_lookup(?FRAME),
  case compile:file(File, [to_core,binary]) of
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
      utils_gui:set_choices(utils:moduleNames(CleanCoreForms)),
      InputSizer = ref_lookup(?INPUT_SIZER),
      wxSizer:layout(InputSizer),
      StartButton = ref_lookup(?START_BUTTON),
      wxButton:enable(StartButton),
      wxFrame:setStatusText(Frame, "Loaded file " ++ File);
    _Other ->
      wxFrame:setStatusText(Frame, "Error: Could not compile file " ++ File)
  end.

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

init_system(Fun, Args) ->
  Proc = #proc{pid = cerl:c_int(1),
               exp = cerl:c_apply(Fun, Args)},
  Procs = [Proc],
  System = #sys{procs = Procs},
  ref_add(?SYSTEM, System),
  Status = ref_lookup(?STATUS),
  NewStatus = Status#status{running = true},
  ref_add(?STATUS, NewStatus).

start(Fun,Args) ->
  Status = ref_lookup(?STATUS),
  #status{loaded = {true, FunDefs}} = Status,
  utils_gui:stop_refs(),
  cauder:start_refs(FunDefs),
  init_system(Fun, Args),
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
  ManualButtons = lists:seq(?FORW_INT_BUTTON, ?BACK_SCH_BUTTON),
  ?LOG("full options: " ++ ?TO_STRING(utils_gui:sort_opts(Options))),
  case string:to_integer(PidText) of
    {error, _} ->
      utils_gui:disable_rule_buttons(ManualButtons);
    {PidInt, _} ->
      FiltOpts = utils:filter_options(Options, PidInt),
      FiltButtons = lists:map(fun utils_gui:option_to_button_label/1, FiltOpts),
      [utils_gui:set_button_label_if(Button, FiltButtons) ||
                               Button <- ManualButtons]
  end,
  HasFwdOptions = utils:has_fwd(Options),
  HasBwdOptions = utils:has_bwd(Options),
  HasNormOptions = utils:has_norm(Options),
  utils_gui:set_ref_button_if(?FORWARD_BUTTON, HasFwdOptions),
  utils_gui:set_ref_button_if(?BACKWARD_BUTTON, HasBwdOptions),
  utils_gui:set_ref_button_if(?NORMALIZE_BUTTON, HasNormOptions).

disable_all_buttons() ->
  ForwIntButton   = ref_lookup(?FORW_INT_BUTTON),
  ForwSchButton   = ref_lookup(?FORW_SCH_BUTTON),
  BackIntButton   = ref_lookup(?BACK_INT_BUTTON),
  BackSchButton   = ref_lookup(?BACK_SCH_BUTTON),
  ForwardButton   = ref_lookup(?FORWARD_BUTTON),
  BackwardButton  = ref_lookup(?BACKWARD_BUTTON),
  NormalizeButton = ref_lookup(?NORMALIZE_BUTTON),
  %RollButton      = ref_lookup(?ROLL_BUTTON),
  wxButton:disable(ForwIntButton),
  wxButton:disable(ForwSchButton),
  wxButton:disable(BackIntButton),
  wxButton:disable(BackSchButton),
  wxButton:disable(ForwardButton),
  wxButton:disable(BackwardButton),
  wxButton:disable(NormalizeButton).
  %wxButton:disable(RollButton).

refresh(RefState) ->
  case utils_gui:is_app_running() of
    false -> ok;
    true ->
      System = ref_lookup(?SYSTEM),
      Options = cauder:eval_opts(System),
      refresh_buttons(Options),
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
      end
  end.

start() ->
  InputTextCtrl = ref_lookup(?INPUT_TEXT),
  InputText = wxTextCtrl:getValue(InputTextCtrl),
  FunChoice = ref_lookup(?FUN_CHOICE),
  NumChoice = wxChoice:getSelection(FunChoice),
  StringChoice = wxChoice:getString(FunChoice, NumChoice),
  Fun = utils:stringToFunName(StringChoice),
  Args = utils:stringToCoreArgs(InputText),
  {_, FunArity} = cerl:var_name(Fun),
  case FunArity == length(Args) of
    true ->
      start(Fun, Args),
      ?LOG("start fun " ++ StringChoice ++ " with args " ++ InputText);
    false ->
      utils_gui:update_status_text(?ERROR_NUM_ARGS),
      error
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

eval_mult(Button) ->
  System = ref_lookup(?SYSTEM),
  StepTextCtrl = ref_lookup(?STEP_TEXT),
  StepText = wxTextCtrl:getValue(StepTextCtrl),
  case string:to_integer(StepText) of
    {error, _} ->
      error;
    {Steps, _} ->
      Option =
        case Button of
          ?FORWARD_BUTTON -> ?MULT_FWD;
          ?BACKWARD_BUTTON -> ?MULT_BWD
        end,
      {NewSystem, StepsDone} = cauder:eval_mult(System, Option, Steps),
      ref_add(?SYSTEM, NewSystem),
      {StepsDone, Steps}
  end.

eval_norm() ->
  System = ref_lookup(?SYSTEM),
  {NewSystem, StepsDone} = cauder:eval_norm(System),
  ref_add(?SYSTEM, NewSystem),
  StepsDone.

eval_roll() ->
  System = ref_lookup(?SYSTEM),
  PidTextCtrl = ref_lookup(?ROLL_PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  StepTextCtrl = ref_lookup(?ROLL_STEP_TEXT),
  StepText = wxTextCtrl:getValue(StepTextCtrl),
  {Pid, _} = string:to_integer(PidText),
  {Steps, _} = string:to_integer(StepText),
  case {Pid, Steps} of
    {error, _} -> error;
    {_, error} -> error;
    _ ->
      CorePid = cerl:c_int(Pid),
      {NewSystem, StepsDone} = cauder:eval_roll(System, CorePid, Steps),
      ref_add(?SYSTEM, NewSystem),
      {StepsDone, Steps}
  end.

eval_roll_send() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_SEND_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    error -> error;
    _ ->
      NewSystem = cauder:eval_roll_send(System, Id),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_roll_spawn() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_SPAWN_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    error -> error;
    _ ->
      NewSystem = cauder:eval_roll_spawn(System, cerl:c_int(Id)),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_roll_rec() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_REC_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  {Id, _} = string:to_integer(IdText),
  case Id of
    error -> error;
    _ ->
      NewSystem = cauder:eval_roll_rec(System, Id),
      ref_add(?SYSTEM, NewSystem)
  end.

eval_roll_var() ->
  System = ref_lookup(?SYSTEM),
  IdTextCtrl = ref_lookup(?ROLL_VAR_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  case IdText of
    "" -> error;
    _ ->
      NewSystem = cauder:eval_roll_var(System, cerl:c_var(list_to_atom(IdText))),
      ref_add(?SYSTEM, NewSystem)
  end.

focus_roll_log() ->
  RBotNotebook = ref_lookup(?RBOT_NOTEBOOK),
  wxNotebook:setSelection(RBotNotebook, ?PAGEPOS_ROLL).

loop() ->
    receive
        %% ------------------- Button handlers ------------------- %%
        #wx{id = ?START_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          start(),
          loop();
        #wx{id = ?NORMALIZE_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          disable_all_buttons(),
          StepsDone = eval_norm(),
          utils_gui:sttext_norm(StepsDone),
          refresh(true),
          loop();
        #wx{id = ?ROLL_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          disable_all_buttons(),
          eval_roll(),
          focus_roll_log(),
          refresh(true),
          loop();
        #wx{id = RuleButton, event = #wxCommand{type = command_button_clicked}}
          when (RuleButton >= ?FORW_INT_BUTTON) and (RuleButton =< ?BACK_SCH_BUTTON) ->
          disable_all_buttons(),
          exec_with(RuleButton),
          utils_gui:sttext_single(RuleButton),
          refresh(true),
          loop();
        #wx{id = RuleButton, event = #wxCommand{type = command_button_clicked}}
          when (RuleButton == ?FORWARD_BUTTON) or (RuleButton == ?BACKWARD_BUTTON) ->
          disable_all_buttons(),
          case eval_mult(RuleButton) of
            error ->
              utils_gui:update_status_text(?ERROR_NUM_STEP);
            {StepsDone, TotalSteps} ->
              utils_gui:sttext_mult(StepsDone, TotalSteps)
          end,
          refresh(true),
          loop();
        #wx{id = ?ROLL_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          disable_all_buttons(),
          eval_roll_send(),
          focus_roll_log(),
          refresh(true),
          loop();
        #wx{id = ?ROLL_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          disable_all_buttons(),
          eval_roll_spawn(),
          focus_roll_log(),
          refresh(true),
          loop();
        #wx{id = ?ROLL_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          disable_all_buttons(),
          eval_roll_rec(),
          focus_roll_log(),
          refresh(true),
          loop();
        #wx{id = ?ROLL_VAR_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          disable_all_buttons(),
          eval_roll_var(),
          focus_roll_log(),
          refresh(true),
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
