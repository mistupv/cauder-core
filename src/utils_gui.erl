-module(utils_gui).
-export([is_app_loaded/0, is_app_running/0,
         option_to_button_label/1, button_to_option/1,
         disable_rule_buttons/1, set_button_label_if/2, set_ref_button_if/2,
         set_choices/1, stop_refs/0, update_status_text/1,
         sttext_single/1, sttext_mult/2, sttext_norm/1,
         prev_font_size/1, next_font_size/1, sort_opts/1, toggle_opts/0,
         pp_marked_text/2]).

-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").

is_app_loaded() ->
  Status = ref_lookup(?STATUS),
  #status{loaded = LoadedStatus} = Status,
  case LoadedStatus of
    {true, _} -> true;
    _Other -> false
  end.

is_app_running() ->
  Status = ref_lookup(?STATUS),
  #status{running = RunningStatus} = Status,
  RunningStatus.

get_label_from_option(Option) ->
  case Option of
    #opt{rule = ?RULE_SEQ}     -> "Seq";
    #opt{rule = ?RULE_SEND}    -> "Send";
    #opt{rule = ?RULE_RECEIVE} -> "Receive";
    #opt{rule = ?RULE_SPAWN}   -> "Spawn";
    #opt{rule = ?RULE_SELF}    -> "Self";
    #opt{rule = ?RULE_SCHED}   -> ?NULL_LABEL
  end.

get_rule_from_button(Button) ->
  Label = wxButton:getLabel(ref_lookup(Button)),
  case Label of
     "Seq"     -> ?RULE_SEQ;
     "Send"    -> ?RULE_SEND;
     "Receive" -> ?RULE_RECEIVE;
     "Spawn"   -> ?RULE_SPAWN;
     "Self"    -> ?RULE_SELF
  end.

button_to_option(Button) ->
  case Button of
    ?FORW_INT_BUTTON ->
      Rule = get_rule_from_button(Button),
      #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = Rule};
    ?FORW_SCH_BUTTON ->
      #opt{sem = ?FWD_SEM, type = ?TYPE_MSG, rule = ?RULE_SCHED};
    ?BACK_INT_BUTTON ->
      Rule = get_rule_from_button(Button),
      #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = Rule};
    ?BACK_SCH_BUTTON ->
      #opt{sem = ?BWD_SEM, type = ?TYPE_MSG, rule = ?RULE_SCHED}
  end.

option_to_button_label(Option) ->
  #opt{sem = Sem, type = Type} = Option,
  Label = get_label_from_option(Option),
  Button =
    case Sem of
      ?FWD_SEM ->
        case Type of
          ?TYPE_MSG  -> ?FORW_SCH_BUTTON;
          ?TYPE_PROC -> ?FORW_INT_BUTTON
        end;
      ?BWD_SEM ->
        case Type of
          ?TYPE_MSG  -> ?BACK_SCH_BUTTON;
          ?TYPE_PROC -> ?BACK_INT_BUTTON
        end
    end,
  {Button, Label}.

disable_rule_buttons(Buttons) ->
  [wxButton:disable(ref_lookup(Button)) || Button <- Buttons].

set_button_label_if(Button, EnabledButtonLabels) ->
  RefButton = ref_lookup(Button),
  case lists:keyfind(Button, 1, EnabledButtonLabels) of
    false ->
      wxButton:disable(RefButton),
      case Button of
        ?FORW_INT_BUTTON -> wxButton:setLabel(RefButton, "Seq");
        ?BACK_INT_BUTTON -> wxButton:setLabel(RefButton, "Seq");
        _Other -> ok
      end;
    {Button, Label} ->
      wxButton:enable(RefButton),
      case Label of
        ?NULL_LABEL -> ok;
        Label -> wxButton:setLabel(RefButton, Label)
      end
  end.

set_ref_button_if(Ref, Cond) ->
  RefButton = ref_lookup(Ref),
  case Cond of
    true ->
      wxButton:enable(RefButton);
    false ->
      wxButton:disable(RefButton)
  end.

set_choices(Choices) ->
  FunChoice = ref_lookup(?FUN_CHOICE),
  wxChoice:clear(FunChoice),
  [wxChoice:append(FunChoice, Choice) || Choice <- Choices],
  wxChoice:setSelection(FunChoice, 0).

stop_refs() ->
  case is_app_running() of
    true ->
      cauder:stop_refs(),
      ok;
    false -> ok
  end.

sttext_single(Button) ->
  Option = button_to_option(Button),
  #opt{sem = Sem} = Option,
  SemStr =
    case Sem of
      ?FWD_SEM -> " forward ";
      ?BWD_SEM -> " backward "
    end,
  Label = get_label_from_option(Option),
  LabelStr =
    case Label of
      ?NULL_LABEL -> "Sched";
      _Other -> Label
    end,
  FullStr = "Fired" ++ SemStr ++ LabelStr ++ " rule",
  update_status_text(FullStr).

sttext_norm(Steps) ->
  StepsStr = integer_to_list(Steps),
  FullStr = StepsStr ++ " steps done",
  update_status_text(FullStr).

sttext_mult(StepsDone, Steps) ->
  StepsDoneStr = integer_to_list(StepsDone),
  StepsStr = integer_to_list(Steps),
  FullStr = StepsDoneStr ++ " of " ++ StepsStr ++ " steps done",
  update_status_text(FullStr).

update_status_text(String) ->
  Frame = ref_lookup(?FRAME),
  wxFrame:setStatusText(Frame, String).

index_of(Elem, List) -> index_of(Elem, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Elem, [Elem|_], Index) -> Index;
index_of(Elem, [_|Rest], Index) -> index_of(Elem, Rest, Index + 1).

prev_font_size(CurSize) ->
  SizeIdx = index_of(CurSize, ?FONT_SIZES),
  case SizeIdx == 1 of
    true  -> CurSize;
    false -> lists:nth(SizeIdx - 1, ?FONT_SIZES)
  end.

next_font_size(CurSize) ->
  SizeIdx = index_of(CurSize, ?FONT_SIZES),
  SizeLen = length(?FONT_SIZES),
  case SizeIdx == SizeLen of
    true  -> CurSize;
    false -> lists:nth(SizeIdx + 1, ?FONT_SIZES)
  end.

sort_opts(Opts) ->
  SortOpts = lists:sort(fun(P1, P2) -> P1#opt.id < P2#opt.id end, Opts),
  SortOpts.

toggle_opts() ->
  MenuView = ref_lookup(?MENU_VIEW),
  [{?PRINT_MAIL, wxMenu:isChecked(MenuView, ?TOGGLE_MAIL)},
   {?PRINT_HIST, wxMenu:isChecked(MenuView, ?TOGGLE_HIST)},
   {?PRINT_ENV,  wxMenu:isChecked(MenuView, ?TOGGLE_ENV)},
   {?PRINT_EXP,  wxMenu:isChecked(MenuView, ?TOGGLE_EXP)},
   {?PRINT_FULL, wxMenu:isChecked(MenuView, ?RADIO_FULL)}].

marked(Ctrl, [], Acc) ->
  wxTextCtrl:appendText(Ctrl, Acc);
marked(Ctrl, [{Attr, Text}|Rest], Acc) ->
  wxTextCtrl:appendText(Ctrl, Acc),
  TextAttr = wxTextAttr:new(Attr),
  Start = wxTextCtrl:getInsertionPoint(Ctrl),
  wxTextCtrl:appendText(Ctrl, Text),
  End = wxTextCtrl:getInsertionPoint(Ctrl),
  wxTextCtrl:setStyle(Ctrl, Start, End, TextAttr),
  marked(Ctrl, Rest, "");
marked(Ctrl, [Text|Rest], Acc) ->
  marked(Ctrl, Rest, Acc ++ [Text]).

pp_marked_text(Ctrl, TextList) ->
  % Freeze control when inserting text
  wxTextCtrl:freeze(Ctrl),
  wxTextCtrl:clear(Ctrl),
  marked(Ctrl, TextList, ""),
  % Put scroll back at the top
  wxTextCtrl:setInsertionPoint(Ctrl, 0),
  % Unfreeze control
  wxTextCtrl:thaw(Ctrl).

ref_lookup(Id) ->
    ets:lookup_element(?GUI_REF, Id, 2).
