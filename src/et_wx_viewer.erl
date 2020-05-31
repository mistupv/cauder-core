%IMPORTANT 
%% THIS FILE IS MODIFIED FROM et_wx_viewer.erl OF OTP STANDARD LIBRARY FOR PARTIAL-ORDER GRAPHIC CHART
%%UNDER COPYRIGHT OF ERICSSON AB: 
%%      %CopyrightBegin%
%%
%%      Copyright Ericsson AB 2000-2016. All Rights Reserved.
%%
%%      Licensed under the Apache License, Version 2.0 (the "License");
%%      you may not use this file except in compliance with the License.
%%      You may obtain a copy of the License at
%%
%%      http://www.apache.org/licenses/LICENSE-2.0
%%
%%      Unless required by applicable law or agreed to in writing, software
%%      distributed under the License is distributed on an "AS IS" BASIS,
%%      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%      See the License for the specific language governing permissions and
%%      limitations under the License.
%%
%%      %CopyrightEnd%
%IMPORTANT END
%%IMPORTANT END

%MyCopyright
%Copyright [Ivan Lanese] [Luca Tabanelli]

%Licensed under the Apache License, Version 2.0 (the "License");
%you may not use this file except in compliance with the License.
%You may obtain a copy of the License at

%    http://www.apache.org/licenses/LICENSE-2.0

%Unless required by applicable law or agreed to in writing, software
%distributed under the License is distributed on an "AS IS" BASIS,
%WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%See the License for the specific language governing permissions and
%limitations under the License.
%MyCopyrightEnd



%%
%% %OriginOperaCopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %OriginOperaCopyrightEnd%


-module(et_wx_viewer).
-behaviour(gen_server).
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("et/include/et.hrl").
-include_lib("et/src/et_internal.hrl").
-include_lib("wx/include/wx.hrl").

-define(unknown, "UNKNOWN").
-define(initial_x, 10).
-define(incr_x,    60).
-define(initial_y, 15).
-define(incr_y,    15).

-record(state,
        {parent_pid,           % Pid of parent process
         auto_shutdown,        % Shutdown collector when last subscriber dies
         collector_pid,        % Pid of collector process
         event_order,          % Field to be used as primary key
         trace_pattern,        % Collector trace pattern
         active_filter,        % Name of the active filter
         filters,              % List of possible filters
	   filter_menu,
         pending_actor,        % Pending actor - move or toggle
         first_event,          % Key of first event (regardless of visibility)
         last_event,           % Key of last event (regardless of visibility)
         events_per_page,      % Maximum number of shown events
         events,               % Queue containg all event keys (regardless of visibility)
	 n_events,             % Number of events available in the collector
         max_actors,           % Maximum number of shown actors
         actors,               % List of known actors
         refresh_needed,       % Refresh is needed in order to show all actors
         detail_level,         % Show only events with lesser detail level
         hide_actions,         % Hide/show events where to == from actor (bool)
         hide_actors,          % Hide/show events with unknown actor (bool)
	 display_all,
     async,      
     async_patterns,
	 context,              % display | print
         title,                % GUI: Window title
         frame,                % GUI: Window object
         menubar,              % GUI: Menu bar object
         packer,               % GUI: Packer object
         width,                % GUI: Window width
         height,               % GUI: Window height
         scale,                % GUI: Scaling factor on canvas
         normal_font,          % GUI: Font to be used on text labels
         bold_font,            % GUI: Font to be used on text labels
	 pen,
	 brush,
	 print_psdd,
	 print_d,
         canvas_width,         % GUI: Canvas width
         canvas_height,        % GUI: Canvas height
         canvas,               % GUI: Canvas object
	 canvas_sizer,
	 scroll_bar,           % GUI: Canvas scroll bar
         y_pos,                % GUI: Current y position on canvas
	 menu_data,
	 checkbox_data,
	 hide_actions_box,
	 hide_actors_box,
	 status_bar,
	 event_file,
	 wx_debug,             % GUI: WX debug level
	 trap_exit}).          % trap_exit process flag


-record(actor, {name, string, include, exclude}).
-record(e, {pos, key, event}).

start_link(Options) -> 
    case parse_opt(Options, default_state(), []) of
        {ok, S, CollectorOpt} ->
            case S#state.collector_pid of
                CollectorPid when is_pid(CollectorPid) ->
		        case gen_server:start_link(?MODULE, [S], []) of
			         {ok, Pid} when S#state.parent_pid =/= self() ->
			                unlink(Pid),
			                 {ok, Pid};
			         Other ->
			             Other
		        end;
                undefined ->
                    case et_collector:start_link([{auto_shutdown, true} | CollectorOpt]) of
                        {ok, CollectorPid} ->
                            S2 = S#state{collector_pid = CollectorPid},
                            case gen_server:start_link(?MODULE, [S2], []) of
                                {ok, Pid} when S#state.parent_pid =/= self() ->
                                    unlink(Pid),
                                    {ok, Pid};
                                Other ->
                                    Other
                            end;
                        {error, Reason} ->
                            {error, {et_collector, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

default_state() ->
    #state{parent_pid          = self(),
           collector_pid       = undefined,
	   n_events            = 0,
           detail_level        = ?detail_level_max,
           active_filter       = ?DEFAULT_FILTER_NAME,
           filters             = [?DEFAULT_FILTER],
           event_order         = trace_ts,
           events_per_page     = 100,
           first_event         = first,
           last_event          = first,
           async               = false,
           events              = queue_new(),
           max_actors          = 5,
           actors              = [create_actor(?unknown)],
           pending_actor       = ?unknown,
           hide_actions        = false,
           hide_actors         = false,
           display_all         = true,
	   context             = display,
           refresh_needed      = false,
           scale               = 2,
           canvas_height       = 0,
           canvas_width        = 0,
           width               = 800,
           height              = 600,
	   event_file          = filename:absname("et_viewer.etrace"),
	   wx_debug            = 0,
	   trap_exit           = true}.

parse_opt([], S, CollectorOpt) ->
    {ok, S, [{parent_pid, S#state.parent_pid} | CollectorOpt]};
parse_opt([H | T], S, CollectorOpt) ->
    case H of
        %STORE THE ASYNC PATTERNS SELECTED FROM THE USER WITH ITS HOMOLOGIST LABEL
        {async_patterns,{Spawn,Send,Receive,Exit}} ->
            parse_opt(T, S#state{async_patterns=#{Spawn=>spawn,Send=>send,Receive=>'receive',Exit=>exit}}, CollectorOpt);
        {async_patterns,_Else} ->
            parse_opt(T, S#state{async_patterns=undefined}, CollectorOpt);
        %SELECT IF YOU WANT AN ASYNC BEHAVIOUR OR NOT
        {async,true} ->
            parse_opt(T, S#state{async = true}, CollectorOpt);
        {async,_Otherwise}->
            parse_opt(T, S#state{async=false}, CollectorOpt);
        {parent_pid, Parent} when is_pid(Parent); Parent =:= undefined ->
            parse_opt(T, S#state{parent_pid = Parent}, CollectorOpt);
	{wx_debug, Level} ->
            parse_opt(T, S#state{wx_debug = Level}, CollectorOpt);
	{trap_exit, Bool} when Bool =:= true; Bool =:= false->
            parse_opt(T, S#state{trap_exit = Bool}, CollectorOpt);
        {title, Title} ->
            parse_opt(T, S#state{title = name_to_string(Title)}, CollectorOpt);
        {detail_level, Level} when is_integer(Level),
				   Level >= ?detail_level_min,
				   Level =< ?detail_level_max -> 
            parse_opt(T, S#state{detail_level = Level}, CollectorOpt);
        {detail_level, max} ->
            parse_opt(T, S#state{detail_level = ?detail_level_max}, CollectorOpt);
        {detail_level, min} ->
            parse_opt(T, S#state{detail_level = ?detail_level_min}, CollectorOpt);
        {scale, Scale} when is_integer(Scale), Scale > 0 ->
            parse_opt(T, S#state{scale = Scale}, CollectorOpt);
        {width, W} when is_integer(W), W > 0 ->
            parse_opt(T, S#state{width = W, canvas_width = W}, CollectorOpt);
        {height, WH} when is_integer(WH), WH > 0 ->
            parse_opt(T, S#state{height = WH, canvas_height = WH}, CollectorOpt);
        {collector_pid, Pid} when is_pid(Pid) -> 
            parse_opt(T, S#state{collector_pid = Pid}, CollectorOpt);
        {collector_pid, undefined} -> 
            parse_opt(T, S#state{collector_pid = undefined}, CollectorOpt);
        {active_filter, Name} when is_atom(Name) ->
            parse_opt(T, S#state{active_filter = Name}, CollectorOpt);
        {event_order, trace_ts} -> %% BUGBUG: Verify event_order with collector
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{event_order = trace_ts}, CollectorOpt2);
        {event_order, event_ts} -> %% BUGBUG: Verify event_order with collector
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{event_order = event_ts}, CollectorOpt2);
        {trace_port, _Port} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_max_queue, _Queue} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_pattern, _Pattern} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_global, _Boolean} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_client, _Client} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {dict_insert, {filter, Name}, Fun} ->
	    if
		is_atom(Name), is_function(Fun) ->
		    F = #filter{name = Name, function = Fun},
		    Filters = lists:keydelete(Name, #filter.name, S#state.filters),
		    CollectorOpt2 = [H | CollectorOpt],
		    parse_opt(T, S#state{filters = Filters ++ [F]}, CollectorOpt2);
		true ->
	            {error, {bad_option, H}}
	    end;
        {dict_insert, {subscriber, Pid}, _Val} ->
	    if
		is_pid(Pid) ->
		    CollectorOpt2 = [H | CollectorOpt],
		    parse_opt(T, S, CollectorOpt2);
		true ->
	            {error, {bad_option, H}}
	    end;
        {dict_insert, _Key, _Val} ->
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {dict_delete, {filter, Name}} ->
            Filters = lists:keydelete(Name, #filter.name, S#state.filters),
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{filters = Filters}, CollectorOpt2);
        {dict_delete, _Key} ->
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {max_events, _Max} ->
	    %% Kept for backward compatibility
            parse_opt(T,  S, CollectorOpt);
        {max_actors, Max} when is_integer(Max), Max >= 0 ->
            parse_opt(T,  S#state{max_actors = Max}, CollectorOpt);
        {max_actors, Max} when Max =:= infinity ->
            parse_opt(T,  S#state{max_actors = Max}, CollectorOpt);
        {actors, ActorNames} when is_list(ActorNames) ->
            ActorNames2 = 
                case lists:member(?unknown, ActorNames) of
                    false -> [?unknown | ActorNames];
                    true  -> ActorNames
                end,
            Actors = [create_actor(Name) || Name <- ActorNames2],
            parse_opt(T, S#state{actors = Actors}, CollectorOpt);
        {include, ActorNames} when is_list(ActorNames) ->
            Actors = [opt_create_actor(Name, include, S) || Name <- ActorNames],
            parse_opt(T, S#state{actors = Actors}, CollectorOpt);
        {exclude, ActorNames} when is_list(ActorNames) ->
            Actors = [opt_create_actor(Name, exclude, S) || Name <- ActorNames],
            parse_opt(T, S#state{actors = Actors}, CollectorOpt);
        {first_event, _FirstKey} ->
	    %% NYI
            parse_opt(T, S, CollectorOpt);
        {hide_actors, Bool} when Bool =:= true; Bool =:= false ->
            parse_opt(T,  S#state{hide_actors = Bool}, CollectorOpt);
        {hide_actions, Bool} when Bool =:= true; Bool =:= false ->
            parse_opt(T,  S#state{hide_actions = Bool}, CollectorOpt);
	{hide_unknown, Bool} when Bool =:= true; Bool =:= false ->
	    %% Kept for backward compatibility
            parse_opt(T, S, CollectorOpt);	
        {display_mode, _Mode} ->
	    %% Kept for backward compatibility
            parse_opt(T,  S, CollectorOpt);
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, _S, _CollectorOpt) ->
    {error, {bad_option_list, BadList}}.

do_dict_insert({filter, Name}, Fun, S) when is_atom(Name), is_function(Fun) ->
    F = #filter{name = Name, function = Fun},
    Filters = lists:keydelete(Name, #filter.name, S#state.filters),
    Filters2 = lists:keysort(#filter.name, [F | Filters]),
    S2 = create_filter_menu(S, S#state.active_filter, Filters2),    
    S2#state{filters = Filters2};
do_dict_insert(_Key, _Val, S) ->
    S.

do_dict_delete({filter, Name}, S) when is_atom(Name), Name =/= S#state.active_filter ->
    Filters = lists:keydelete(Name, #filter.name, S#state.filters),
    S2 = create_filter_menu(S, S#state.active_filter, Filters),    
    S2#state{filters = Filters};
do_dict_delete(_Key, S) ->
    S.

init([S]) when is_record(S, state) ->
    put(precEventsBar,[]),%%used during scroll of the scroll bar,for store precedent events
    put(up,0),%%used for controlling when i reach first window
    process_flag(trap_exit, S#state.trap_exit),
    case S#state.parent_pid of
	undefined -> ok;
	ParentPid -> link(ParentPid)
    end,
    _ = wx:new(),
    _ = wx:debug(S#state.wx_debug),
    et_collector:dict_insert(S#state.collector_pid,
                             {subscriber, self()},
                             ?MODULE),
    S2 = create_main_window(S),
    EventsPerPage = events_per_page(S2, S2#state.height),
    S3 = revert_main_window(S2#state{events_per_page = EventsPerPage}),
    Timeout = timeout(S3), 
    {ok, S3, Timeout}.

handle_call(get_collector_pid, _From, S) ->
    Reply = S#state.collector_pid,
    reply(Reply, S);
handle_call(stop, _From, S) ->
    wxFrame:destroy(S#state.frame),
    opt_unlink(S#state.parent_pid),
    {stop, shutdown, ok, S};
handle_call({open_event, N}, _From, S) when is_integer(N), N > 0->
    Reply = do_open_event(S, N),
    reply(Reply, S);
    
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~tp, ~tp, ~tp)~n",
			     [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    reply(Reply, S).

handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~tp, ~tp)~n",
			     [?MODULE, self(), Msg, S]),
    noreply(S).

handle_info({et, {more_events, N}}, S) ->
    S4 =
    	if
	    N =:= S#state.n_events ->
		S;
	    true ->
		Missing = S#state.events_per_page - queue_length(S#state.events),
		if
		    Missing =:= 0 ->
			update_scroll_bar(S#state{n_events = N});
		    Missing > 0 ->
			OldEvents = queue_to_list(S#state.events),
			{S2, NewEvents} = 
			    collect_more_events(S#state{n_events = N},
						S#state.last_event,
						Missing),
			S3 = replace_events(S2, OldEvents ++ NewEvents),
			refresh_main_window(S3)
		end
	end,
    noreply(S4);
handle_info({et, {insert_actors, ActorNames}}, S) when is_list(ActorNames) ->
    Fun = fun(N, Actors) ->
		  case lists:keymember(N, #actor.name, Actors) of
		      true  -> Actors;
		      false -> Actors ++ [create_actor(N)]
		  end
	  end,
    Actors = lists:foldl(Fun, S#state.actors, ActorNames),
    S2 = refresh_main_window(S#state{actors = Actors}),
    noreply(S2);
handle_info({et, {delete_actors, ActorNames}}, S) when is_list(ActorNames)->
    Fun = fun(N, Actors) when N =:= ?unknown ->
		  Actors;
	     (N, Actors) ->
		  lists:keydelete(N, #actor.name, Actors)
	  end,
    Actors = lists:foldl(Fun, S#state.actors, ActorNames),
    S2 = refresh_main_window(S#state{actors = Actors}),
    noreply(S2);
handle_info({et, {dict_insert, Key, Val}}, S) ->
    S2 = do_dict_insert(Key, Val, S),
    noreply(S2);
handle_info({et, {dict_delete, Key}}, S) ->
    S2 = do_dict_delete(Key, S),
    noreply(S2);
handle_info({et, first}, S) ->
    S2 = scroll_first(S),
    noreply(S2);
handle_info({et, prev}, S) ->
    S2 = scroll_prev(S),
    noreply(S2);
handle_info({et, next}, S) ->
    S2 = scroll_next(S),
    noreply(S2);
handle_info({et, last}, S) ->
    S2 = scroll_last(S),
    noreply(S2);
handle_info({et, refresh}, S) ->
    S2 = revert_main_window(S),
    noreply(S2);
%%%%handle a message for the clean of the graphic chart(empty actors and events lists of state)
handle_info({et,clean_all},S)->
    case length(queue_to_list(S#state.events)) of
    0->noreply(S);
    _N->
        et_collector:clear_table(S#state.collector_pid),
        {Unknown,_}=lists:split(1,S#state.actors),
        S2=clear_canvas(S#state{actors=Unknown}),
        noreply(S2)
    end;
%%
handle_info({et, {display_mode, _Mode}}, S) ->
    %% Kept for backward compatibility
    noreply(S);
handle_info({et, close}, S) ->
    wxFrame:destroy(S#state.frame),
    opt_unlink(S#state.parent_pid),
    {stop, shutdown, S};
handle_info(#wx{id=?wxID_HELP}, S) ->
    HelpString =
	"Vertical scroll:\n"
	"\tUse mouse wheel and up/down arrows to scroll little.\n"
	"\tUse page up/down and home/end buttons to scroll more.\n\n"
	"Display details of an event:\n"
	"\tLeft mouse click on the event label or the arrow.\n\n"
	"Highlight actor (toggle):\n"
	"\tLeft mouse click on the actor name tag.\n"
	"\tThe actor name will be enclosed in square brackets [].\n\n"
	"Exclude actor (toggle):\n"
	"\tRight mouse click on the actor name tag.\n"
	"\tThe actor name will be enclosed in round brackets ().\n\n"
	"Move actor:\n"
	"\tLeft mouse button drag and drop on actor name tag.\n\n"
	"Display all (reset settings for hidden and/or highlighted actors):\n"
	"\tPress the 'a' button.",
    Dialog =
	wxMessageDialog:new(S#state.frame, HelpString,
			    [{style, 0
				  bor ?wxOK
				  bor ?wxICON_INFORMATION
				  bor ?wxSTAY_ON_TOP},
			     {caption, "Help"}]),
    wxMessageDialog:showModal(Dialog),
    noreply(S);
handle_info(#wx{id=Id, event = #wxCommand{type = command_menu_selected}}, S=#state{filter_menu = {_,Data}}) ->
    CollectorPid = S#state.collector_pid,
    case get_value(Id, 3, S#state.menu_data) of
	close ->
	    wxFrame:destroy(S#state.frame),
	    opt_unlink(S#state.parent_pid),
	    {stop, shutdown, S};
	up ->
	    S2 = scroll_up(S),
	    noreply(S2);
	down ->
	    S2 = scroll_down(S),
	    noreply(S2);
	first ->
	    S2 = scroll_first(S),
	    noreply(S2);
	prev ->
	    S2 = scroll_prev(S),
	    noreply(S2);
	next ->
	    S2 = scroll_next(S),
	    noreply(S2);
	last ->
	    S2 = scroll_last(S),
	    noreply(S2);
	refresh ->
	    S2 = revert_main_window(S),
	    noreply(S2);
	{display_mode, _Mode} ->
	    %% Kept for backward compatibility
	    noreply(S);
	display_all ->
	    S2 = display_all(S),
	    noreply(S2);
	close_all ->
	    close_all(S);
	close_all_others ->
	    close_all_others(S);
	first_all ->
	    et_collector:multicast(CollectorPid, first),
	    noreply(S);
	prev_all ->
	    et_collector:multicast(CollectorPid, prev),
	    noreply(S);
	next_all ->
	    et_collector:multicast(CollectorPid, next),
	    noreply(S);
	last_all ->
	    et_collector:multicast(CollectorPid, last),
	    noreply(S);
	refresh_all ->
	    et_collector:multicast(CollectorPid, refresh),
	    noreply(S);
	clear_all ->
	    et_collector:clear_table(CollectorPid),
	    et_collector:multicast(CollectorPid, refresh),
	    noreply(S);
	load_all ->
	    Style = ?wxFD_OPEN bor ?wxFD_OVERWRITE_PROMPT,
	    Msg = "Select a file to load events from",
            S2 = case select_file(S#state.frame, Msg, S#state.event_file, Style) of
                     {ok, NewFile} ->
                         _ = et_collector:start_trace_client(CollectorPid, event_file, NewFile),
                         S#state{event_file = NewFile};
                     cancel ->
                         S
                 end,
	    noreply(S2);
	save_all ->
	    Style = ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT,
	    Msg = "Select a file to save events to",
            S2 = case select_file(S#state.frame, Msg, S#state.event_file, Style) of
                     {ok, NewFile} ->
                         ok = et_collector:save_event_file(CollectorPid, NewFile,
                                                           [existing, write, keep]),
                         S#state{event_file = NewFile};
                     cancel ->
                         S
                 end,
	    noreply(S2);
	print_setup ->
	    S2 = print_setup(S),
	    noreply(S2);
	print_one_page = Scope ->
	    S2 = print(S, Scope),
	    noreply(S2);
	print_all_pages = Scope ->
	    S2 = print(S, Scope),
	    noreply(S2);
	{open_viewer, Scale} ->
	    Actors = [A#actor.name || A <- S#state.actors],
	    open_viewer(Scale, S#state.active_filter, Actors, S),
	    noreply(S);

	_ ->
	    case get_value(Id, 3, Data) of
		{data, F=#filter{}, Scale} ->
		    open_viewer(S#state.scale+Scale, F#filter.name, [?unknown], S);
		{data, F=#filter{}} ->
		    open_viewer(S#state.scale, F#filter.name, [?unknown], S);
		false ->
		    ok
	    end,
	    noreply(S)
    end;
handle_info(#wx{event = #wxCommand{type = command_slider_updated, commandInt = Level}}, S) ->
    if
	Level >= ?detail_level_min,
	Level =< ?detail_level_max ->
	    S2 = S#state{detail_level = Level},
	    S3 = revert_main_window(S2),
	    noreply(S3);

	true ->
	    noreply(S)
    end;
handle_info(#wx{id = Id, event = #wxCommand{type = command_checkbox_clicked, commandInt = Int}}, S) ->
    case get_value(Id, 2, S#state.checkbox_data) of
	hide_actions ->
	    case Int of
		1 ->
		    S2 = S#state{hide_actions = true},
		    S3 = revert_main_window(S2),
		    noreply(S3);
		0 ->
		    S2 = S#state{hide_actions = false},
		    S3 = revert_main_window(S2),
		    noreply(S3)
	    end;
	hide_actors ->
	    case Int of
		1 ->
		    S2 = S#state{hide_actors = true},
		    S3 = revert_main_window(S2),
		    noreply(S3);
		0 ->
		    S2 = S#state{hide_actors = false},
		    S3 = revert_main_window(S2),
		    noreply(S3)
	    end;
	false ->
	    noreply(S)
    end;
handle_info(#wx{event = #wxMouse{type = left_down, x = X, y = Y}}, S) ->
    S3 =
	case y_to_n(Y, S) of
	    actor ->
		%% Actor click
		case S#state.actors of
		    [] ->
			S;
		    Actors ->
			N = x_to_n(X, S),
			A = lists:nth(N, Actors),
			S#state{pending_actor = A}
		end;
	    {event, N} ->
		%% Event click
		do_open_event(S, N),
		S
	end,
    noreply(S3);
handle_info(#wx{event = #wxMouse{type = left_up}}, S) when S#state.pending_actor =:= undefined ->
    noreply(S);
handle_info(#wx{event = #wxMouse{type = left_up, x = X, y = Y}}, S) ->
    S3 =
	case y_to_n(Y, S) of
	    actor ->
		%% Actor click
		case S#state.actors of
		    [] ->
			S;
		    Actors ->
			N = x_to_n(X, S),
			A = lists:nth(N, Actors),
			Pending = S#state.pending_actor,
			if
			    A#actor.name =:= Pending#actor.name ->
				%% Toggle include actor
				A2 = A#actor{include = not A#actor.include},
				%% io:format("include ~p: ~p -> ~p\n",
				%% [A#actor.name, A#actor.include, A2#actor.include]),
				Actors2 = lists:keyreplace(A#actor.name, #actor.name, Actors, A2),
				DisplayAll = not lists:keymember(true, #actor.include, Actors2),
				S2 = S#state{actors = Actors2, display_all = DisplayAll},
				revert_main_window(S2);
			    true ->
				move_actor(Pending, A, Actors, S)
			end
		end;
	    {event, _N} ->
		%% Event click ignored
		S
	end,
    noreply(S3#state{pending_actor = undefined});
handle_info(#wx{event = #wxMouse{type = right_up, x = X, y = Y}}, S) ->
    S3 =
	case y_to_n(Y, S) of
	    actor ->
		%% Actor click
		case S#state.actors of
		    [] ->
			S;
		    Actors ->
			%% Toggle exclude actor
			N = x_to_n(X, S),
			A = lists:nth(N, Actors),
			A2 = A#actor{exclude = not A#actor.exclude},
			Actors2 = lists:keyreplace(A#actor.name, #actor.name, Actors, A2),
			S2 = S#state{actors = Actors2},
			revert_main_window(S2)
		end;
	    {event, _N} ->
		%% Event click ignored
		S
	end,
    noreply(S3#state{pending_actor = undefined});
handle_info(#wx{event = #wxKey{keyCode = KeyCode, shiftDown = SD}}, S) ->
    case KeyCode of
	$C when SD =:= true ->
	    close_all(S);
	$c ->
	    close_all_others(S);
	?WXK_HOME ->
	    S2 = scroll_first(S),
	    noreply(S2);
	?WXK_END ->
	    S2 = scroll_last(S),
	    noreply(S2);
	?WXK_UP ->
	    S2 = scroll_up(S),
	    noreply(S2);
	?WXK_DOWN ->
	    S2 = scroll_down(S),
	    noreply(S2);
	?WXK_PAGEUP ->
	    S2 = scroll_prev(S),
	    noreply(S2);
	?WXK_PAGEDOWN ->
	    S2 = scroll_next(S),
	    noreply(S2);
	$F when SD =:= true ->
	    et_collector:multicast(S#state.collector_pid, first),
	    noreply(S);
	$F ->
	    S2 = scroll_first(S),
	    noreply(S2);
	$P when SD =:= true ->
	    et_collector:multicast(S#state.collector_pid, prev),
	    noreply(S);
	$P ->
	    S2 = scroll_prev(S),
	    noreply(S2);
	$N when SD =:= true ->
	    et_collector:multicast(S#state.collector_pid, next),
	    noreply(S);
	$N ->
	    S2 = scroll_next(S),
	    noreply(S2);
	$L when SD =:= true ->
	    et_collector:multicast(S#state.collector_pid, last),
	    noreply(S);
	$L ->
	    S2 = scroll_last(S),
	    noreply(S2);
	$R when SD =:= true ->
	    et_collector:multicast(S#state.collector_pid, refresh),
	    noreply(S);
	$R ->
	    S2 = revert_main_window(S),
	    noreply(S2);
	$A ->
	    S2 = display_all(S),
	    noreply(S2);
	$= ->
	    Scale = S#state.scale,
	    Actors = [A#actor.name || A <- S#state.actors],
	    open_viewer(Scale, S#state.active_filter, Actors, S),
	    noreply(S);
	Int when Int =:= $+; Int =:= ?WXK_NUMPAD_ADD ->
	    Scale = S#state.scale + 1,
	    Actors = [A#actor.name || A <- S#state.actors],
	    open_viewer(Scale, S#state.active_filter, Actors, S),
	    noreply(S);
	Int when Int =:= $-; Int =:= ?WXK_NUMPAD_SUBTRACT ->
	    case S#state.scale of
		1 ->
		    ignore;
		Scale ->
		    Actors = [A#actor.name || A <- S#state.actors],
		    open_viewer(Scale - 1, S#state.active_filter, Actors, S)
	    end,
	    noreply(S);
	$0 ->
	    case lists:keysearch(?DEFAULT_FILTER_NAME, #filter.name, S#state.filters) of
		{value, F} when is_record(F, filter) ->
		    open_viewer(S#state.scale, F#filter.name, [?unknown], S);
		false ->
		    ok
	    end,
	    noreply(S);
	Int when is_integer(Int), Int > $0, Int =< $9 ->
	    case catch lists:nth(Int-$0, S#state.filters) of
		F when is_record(F, filter) ->
		    open_viewer(S#state.scale, F#filter.name, [?unknown], S);
		{'EXIT', _} ->
		    ok
	    end,
	    noreply(S);

	_ ->
	    noreply(S)
    end;
handle_info(#wx{event = #wxScroll{type = scroll_changed}} = Wx, S) ->
    _ = get_latest_scroll(Wx),
    Pos = wxScrollBar:getThumbPosition(S#state.scroll_bar),
    {_, LineTopY, LineBotY} = calc_y(S),
    Range = LineBotY - LineTopY,
    N = round(S#state.n_events * Pos / Range),
    Diff = 
	case N - event_pos(S) of
	    D when D < 0 -> D;
	    D            -> D
	end,
    S2 = scroll_changed(S, Diff),
    case ((S2#state.async) and (Diff /=0)) of%% if i did not moved the scroll bar
        true->
            Spawn=element(1,getAsyncPatternsKeys(S2#state.async_patterns)),%%obtain key of spawn async pattern
            case Diff > 0 of
                true->%in case of scroll down
                    put(up,get(up)+Diff),%i must be synchronized with depth (Diff) of scrolling
                    DC = wxClientDC:new(S2#state.canvas), 
                    OldSpawns=[Event||Event<-queue_to_list(S#state.events),Event#e.event#event.label==Spawn],% obtain old spawns
                    put(precEventsBar,get(precEventsBar)++OldSpawns),%store the old spawns
                    [keepAlive(S2,OldSpawn,DC)||OldSpawn<-get(precEventsBar)],
                    wxClientDC:destroy(DC);
                false->%in case of scroll up
                    case abs(Diff) > get(up) of
                        true->
                            put(up,0);
                        false->
                            put(up,get(up)+Diff)%i must be synchronized with depth (Diff) of scrolling
                    end,
                    case get(up) > 0 of%if i am at first page,i won't redrawn spawns
                        true->
                            DC = wxClientDC:new(S2#state.canvas), 
                            NewSpawns=[Event||Event<-queue_to_list(S2#state.events),Event#e.event#event.label==Spawn],% obtain new spawns
                            put(precEventsBar,get(precEventsBar)--NewSpawns),%i prun the new spawns
                            [keepAlive(S2,OldSpawn,DC)||OldSpawn<-get(precEventsBar)],
                            wxClientDC:destroy(DC);
                        false->ok
                    end
            end;
        false->ok
    end,
    noreply(S2);
handle_info(timeout, S) ->
    noreply(S);
handle_info({'EXIT', Pid, Reason}, S) ->
    if
	Pid =:= S#state.collector_pid ->
	    io:format("collector died: ~tp\n\n", [Reason]),
	    wxFrame:destroy(S#state.frame),
	    {stop, Reason, S};
	Pid =:= S#state.parent_pid ->
	    wxFrame:destroy(S#state.frame),
	    {stop, Reason, S};
	true ->
	    noreply(S)
    end;
handle_info(#wx{event = #wxClose{}}, S) ->
    opt_unlink(S#state.parent_pid),
    {stop, shutdown, S};
handle_info(#wx{event = #wxMouse{type = mousewheel, wheelRotation = Rot}}, S) when Rot > 0 ->
    S2 = scroll_up(S),
    case calc_scroll(S2) > get(up) of
        true->put(up,0);%%limit inf. to active redrawn of actors
        _->put(up,get(up)-calc_scroll(S2))
    end,
    case ((S2#state.async) and (get(up) > 0)) of
        true->%if true,i have to keep alive process already alive,after scrolling
            Spawn=element(1,getAsyncPatternsKeys(S2#state.async_patterns)),%%obtain key of spawn async pattern
            DC = wxClientDC:new(S2#state.canvas),
            PrecEvents=get(precEventsBar)--queue_to_list(S2#state.events),%delete events that won't be shown after scrolling
            put(precEventsBar,PrecEvents),%%store new prec events
            [keepAlive(S2,EventSpawn,DC)||EventSpawn<-get(precEventsBar),EventSpawn#e.event#event.label==Spawn],%exec foreach spawn
            wxClientDC:destroy(DC);
        false->ok
    end,
    noreply(S2);
handle_info(#wx{event = #wxMouse{type = mousewheel, wheelRotation = Rot}}, S) when Rot < 0 ->
    S2 = scroll_down(S),
    ScrollCurrPos=wxScrollBar:getThumbPosition(S2#state.scroll_bar),%%get the current pos of scroll bar
    case (wxScrollBar:getRange(S2#state.scroll_bar)) of%limit sup. to active redrawn of actors,scroll bar in the bottom!
        ScrollCurrPos->
                put(up,get(up));
        _->
            put(up,get(up)+calc_scroll(S2))%else i'll go forward in the scrolling
    end,
    case S2#state.async of
        true->%if true,i have to keep alive process already alive,after scrolling
            Spawn=element(1,getAsyncPatternsKeys(S2#state.async_patterns)),%%obtain key of spawn async pattern
            DC = wxClientDC:new(S2#state.canvas),
            PrecEvents=queue_to_list(S#state.events)--queue_to_list(S2#state.events),%obtain events that won't be shown after scrolling
            put(precEventsBar,get(precEventsBar)++PrecEvents),%%store new prec events
            [keepAlive(S2,EventSpawn,DC)||EventSpawn<-get(precEventsBar),EventSpawn#e.event#event.label==Spawn],%exec foreach spawn
            wxClientDC:destroy(DC);
        false->ok
    end,
    noreply(S2);
handle_info(#wx{event = #wxSize{size = {OldW, OldH}}} = Wx, S) ->
    case get(up) of%%during resize of the windows
        0->ok;%if i am in the first window, i don't redraw spawns
        _->%else,i will redrawn oldest spawn not showed
            DC = wxClientDC:new(S#state.canvas),
            Spawn=element(1,getAsyncPatternsKeys(S#state.async_patterns)),%%obtain key of spawn async pattern
            [keepAlive(S,EventSpawn,DC)||EventSpawn<-get(precEventsBar),EventSpawn#e.event#event.label==Spawn],
            wxClientDC:destroy(DC)
    end,
    #wx{event = #wxSize{type = size, size = {W, H}}} = get_latest_resize(Wx),
    S2 = S#state{width = W, height = H, canvas_width = W, canvas_height = H},
    EventsPerPage = events_per_page(S, H),
    Diff = EventsPerPage - S#state.events_per_page,
    S6 = 
	if
	    OldW =:= W, OldH =:= H, S2#state.events_per_page =:= EventsPerPage ->
		S2;
	    Diff =:= 0 ->
		refresh_main_window(S2);
	    Diff > 0 ->
		OldEvents = queue_to_list(S2#state.events),
		{S3, NewEvents} = collect_more_events(S2, S2#state.last_event, Diff),
		S4 = S3#state{events_per_page = EventsPerPage},
		S5 = replace_events(S4, OldEvents ++ NewEvents),
		refresh_main_window(S5);
	    Diff < 0 ->
		OldEvents = queue_to_list(S2#state.events),
		RevEvents = delete_n(lists:reverse(OldEvents), abs(Diff)),
		S3 = S2#state{events_per_page = EventsPerPage},
		S4 = replace_events(S3, lists:reverse(RevEvents)),
		refresh_main_window(S4)
	end,
    noreply(S6);
handle_info(#wx{event = #wxMouse{type = enter_window}}, S) ->
    wxWindow:setFocus(S#state.canvas), % Get keyboard focus
    noreply(S);
handle_info(#wx{event = #wxPaint{}}, S) ->
    S2 = refresh_main_window(S),
    noreply(S2);
handle_info(#wx{event = #wxMouse{type = T, x=X,y=Y}}, S) ->
    io:format("~tp ~tp\n", [T, {X,Y}]),
    noreply(S);
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~tp, ~tp)~n",[?MODULE, self(), Info, S]),
    noreply(S).

terminate(_Reason, _S) ->
    ignore.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

reply(Reply, S) ->
    Timeout = timeout(S),
    {reply, Reply, S, Timeout}.

noreply(S) ->
    Timeout = timeout(S),
    {noreply, S, Timeout}.

timeout(_S) ->
    infinity.

scroll_first(S) ->
    EventsPerPage = S#state.events_per_page,
    {S2, NewEvents} =
	collect_more_events(S, first, EventsPerPage),
    S3 = 
	case NewEvents of
	    [] ->
		S2;
	    [FirstE | _] ->
		S2#state{first_event = FirstE}
	end,
    S4 = replace_events(S3, NewEvents),
    refresh_main_window(S4).

scroll_last(S) ->
    case collect_more_events(S, last, -1) of
	{_, []} ->
	    scroll_first(S);
	{S2, NewEvents} ->
	    [FirstE | _] = NewEvents,
	    S3 = replace_events(S2#state{first_event = FirstE}, NewEvents),
	    refresh_main_window(S3)
    end.

scroll_prev(S) ->
    scroll_up(S, S#state.events_per_page).

scroll_next(S) ->
    scroll_down(S, S#state.events_per_page).

scroll_up(S) ->
    scroll_up(S, calc_scroll(S)).

scroll_up(S, Expected) ->
    N = queue_length(S#state.events),
    EventsPerPage = S#state.events_per_page,
    Expected2 = adjust_expected(Expected, N, EventsPerPage),
    OldEvents = queue_to_list(S#state.events),
    case collect_more_events(S, S#state.first_event, -Expected2) of
	{_, []} ->
	    S;
	{S2, NewEvents} ->
	    NewN = length(NewEvents),
	    if
		N + NewN > EventsPerPage ->
		    RevAllEvents = lists:reverse(OldEvents, lists:reverse(NewEvents)),
		    TooMany = N + NewN - EventsPerPage,
		    case delete_n(RevAllEvents, TooMany) of
			[] ->
				S;
			[LastE | _] = RevEvents ->
			    Events = lists:reverse(RevEvents),
			    S3 = replace_events(S2#state{last_event = LastE}, Events),
			    refresh_main_window(S3)
		    end;
		true ->
		    Events = NewEvents ++ OldEvents,
		    LastE = lists:last(Events),
		    S3 = replace_events(S2#state{last_event = LastE}, Events),
		    refresh_main_window(S3)
	    end
    end.

scroll_down(S) ->
    scroll_down(S, calc_scroll(S)).

scroll_down(S, Expected) ->
    N = queue_length(S#state.events),
    EventsPerPage = S#state.events_per_page,
    Expected2 = adjust_expected(Expected, N, EventsPerPage),
    OldEvents = queue_to_list(S#state.events),
    case collect_more_events(S, S#state.last_event, Expected2) of
	{_, []} ->
	    case collect_more_events(S, S#state.first_event, N - EventsPerPage) of
		{_, []} ->
		    S;
		{S2, NewEvents} ->
		    Events = NewEvents ++ OldEvents,
		    [FirstE | _] = Events,
		    S3 = replace_events(S2#state{first_event = FirstE}, Events),
		    refresh_main_window(S3)
	    end;
	{S2, NewEvents} ->
	    AllEvents = OldEvents ++ NewEvents,
	    case delete_n(AllEvents, length(NewEvents)) of
		[] ->
		    scroll_first(S);
		Events ->
		    [FirstE | _] = Events,
		    S3 = replace_events(S2#state{first_event = FirstE}, Events),
		    refresh_main_window(S3)
	    end
    end.

scroll_changed(S, Expected) ->
    if
	Expected =:= 0 ->
	    refresh_main_window(S);
	Expected < 0 ->
	    %% Up
	    OldPos = event_pos(S),
	    NewPos = lists:max([OldPos + Expected, 0]),
	    case S#state.first_event of
		#e{key = Key, pos = OldPos} ->
		    jump_up(S, Key, OldPos, NewPos);
		first ->
		    scroll_first(S);
		last ->
		    scroll_last(S)
            end;
	true ->
	    %% Down
	    OldPos = event_pos(S),
	    NewPos = lists:min([OldPos + Expected, S#state.n_events]),
	    case S#state.first_event of
		    #e{key = Key, pos = OldPos} ->
		    jump_down(S, Key, OldPos, NewPos);
		first = Key ->
		    jump_down(S, Key, 0, NewPos);
		last ->
		    scroll_last(S)
	    end
    end.

jump_up(S, OldKey, OldPos, NewPos) ->
    Try = NewPos - OldPos -1,
    Order = S#state.event_order,
    PrevE =
        if NewPos =:= 0 ->
                first;
           true ->
                Fun = fun(Event, #e{pos = P}) when P >= NewPos ->
                              Key = et_collector:make_key(Order, Event),
                              #e{event = Event, key = Key, pos = P - 1};
                         (_E, Acc) ->
                              Acc
                      end,
                et_collector:iterate(S#state.collector_pid,
                                     OldKey,
                                     Try,
                                     Fun,
                                     #e{key = OldKey, pos = OldPos})
        end,
    case collect_more_events(S, PrevE, S#state.events_per_page) of
	{_, []} ->
	    S;
	{S2, Events} ->
	    [FirstE | _] = Events,
	    S3 = replace_events(S2#state{first_event = FirstE}, Events),
	    refresh_main_window(S3)
    end.

jump_down(S, OldKey, OldPos, NewPos) ->
    Try = NewPos - OldPos,
    Order = S#state.event_order,
    Fun = fun(Event, #e{pos = P}) when P < NewPos ->
		  Key = et_collector:make_key(Order, Event),
		  #e{event = Event, key = Key, pos = P + 1};
	     (_, Acc) ->
		  Acc
	  end,
    PrevE = et_collector:iterate(S#state.collector_pid,
				 OldKey, 
				 Try, 
				 Fun,
				 #e{key = OldKey, pos = OldPos}),
    case collect_more_events(S, PrevE, S#state.events_per_page) of
	{_, []} ->
	    S;
	{S2, Events} ->
	    [FirstE | _] = Events,
	    S3 = replace_events(S2#state{first_event = FirstE}, Events),
	    refresh_main_window(S3)
    end.

adjust_expected(Expected, N, EventsPerPage) ->
    if
	N < EventsPerPage ->
	    EventsPerPage - N;
	Expected < EventsPerPage ->
	    Expected;
	true ->
	    EventsPerPage
    end.

calc_scroll(S) ->
    lists:max([S#state.events_per_page div 3, 1]).

revert_main_window(S) ->
    {S2, Events} = revert(S),
    S3 = replace_events(S2, Events),
    refresh_main_window(S3).

revert(S) ->
    EventsPerPage = S#state.events_per_page,
    %% Find previous event
    case collect_more_events(S, S#state.first_event, -1) of
	{_, []} ->
	    collect_more_events(S, first, EventsPerPage);
	{S2, [_PrevEvent]} ->
	    collect_more_events(S, S2#state.first_event, EventsPerPage)
    end.

delete_n(List, 0) ->
    List;
delete_n([], _) ->
    [];
delete_n([_ | Tail], N) when N > 0 ->
    delete_n(Tail, N - 1).

pick_n(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
pick_n([], _N, Acc) ->
    {lists:reverse(Acc), []};
pick_n([Head | Tail], N, Acc) when N > 0 ->
    pick_n(Tail, N - 1, [Head | Acc]).

close_all(S) ->
    _ = close_all_others(S),
    wxFrame:destroy(S#state.frame),
    opt_unlink(S#state.parent_pid),
    {stop, shutdown, S}.

close_all_others(S) ->
    Fun =
	fun({{subscriber, Pid}, _}) ->
		if Pid =:= self() ->
			ok;
		    true ->
			unlink(Pid),
			Pid ! {et, close},
                        ok
		end
	end,
    All = et_collector:dict_match(S#state.collector_pid,
				  {{subscriber, '_'}, '_'}),
    lists:foreach(Fun, All),
    noreply(S).

opt_unlink(Pid) ->
    if
	Pid =:= undefined ->
	    ignore;
	true -> 
	    unlink(Pid)
    end.

open_viewer(Scale, FilterName, Actors, S) ->
    Filters = [{dict_insert, {filter, F#filter.name}, F#filter.function}
	       || F <- S#state.filters],
    Options = 
	[{parent_pid,    S#state.parent_pid},
	 {title,         S#state.title},
	 {collector_pid, S#state.collector_pid},
	 {detail_level,  S#state.detail_level},
	 {active_filter, FilterName},
	 {event_order,   S#state.event_order},
	 {first_event,   S#state.first_event},
	 {max_actors,    S#state.max_actors},
	 {hide_actions,  S#state.hide_actions},
	 {hide_actors,   S#state.hide_actors},
     {async,         S#state.async},
     {async_patterns, getAsyncPatternsKeys(S#state.async_patterns)},
	 {actors,        Actors},
	 {scale,         Scale},
	 {width,         S#state.width},
	 {height,        S#state.height} | Filters],
    case start_link(Options) of
	{ok, _ViewerPid} ->
	    %% unlink(ViewerPid),
	    ok;
	{error, Reason} ->
	    ok = error_logger:format("~p: Failed to start a new window: ~tp~n",
				     [?MODULE, Reason])
    end.

%%%----------------------------------------------------------------------
%%% Handle graphics
%%%----------------------------------------------------------------------
create_main_window(S) ->
    {NormalFont, BoldFont} = select_fonts(S#state.scale),
    Name    = name_to_string(S#state.active_filter),
    Title   = case S#state.title of
		  undefined -> atom_to_list(?MODULE);
		  Explicit  -> name_to_string(Explicit)
	      end,
    Frame   = wxFrame:new(wx:null(), 
			  ?wxID_ANY, 
			  Title ++ " (filter: " ++ Name ++  ")", 
			  [{size, {S#state.width, S#state.height}}]),
    StatusBar = wxFrame:createStatusBar(Frame),

    Panel   = wxPanel:new(Frame, []),
    Bar     = wxMenuBar:new(),
    wxFrame:setMenuBar(Frame,Bar),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    MenuData = lists:flatten([create_file_menu(Bar),
			      create_viewer_menu(Bar),
			      create_collector_menu(Bar)]),
    FilterMenu = wxMenu:new([]),
    S2 = create_filter_menu(S#state{filter_menu = {FilterMenu,[]}},
			    S#state.active_filter,
			    S#state.filters),
    wxMenuBar:append(Bar, FilterMenu, "Filters and scaling"),
    create_help_menu(Bar),

    OptSizer     = wxBoxSizer:new(?wxHORIZONTAL),
    CheckSizer   = wxBoxSizer:new(?wxVERTICAL),
    HideActions  = wxCheckBox:new(Panel, ?wxID_ANY, "Hide From=To"),
    wxCheckBox:setValue(HideActions, S#state.hide_actions),
    HideActors   = wxCheckBox:new(Panel, ?wxID_ANY, "Hide (excluded actors)"),
    wxCheckBox:setValue(HideActors, S#state.hide_actors),
    CheckBoxData = [{wxCheckBox:getId(HideActions), hide_actions},
		    {wxCheckBox:getId(HideActors), hide_actors}],
    wxPanel:connect(Panel, command_checkbox_clicked),
    _ = wxSizer:add(CheckSizer, HideActions),
    _ = wxSizer:add(CheckSizer,HideActors),
    _ = wxSizer:add(OptSizer, CheckSizer, [{border, 10}, {flag, ?wxALL}]),
    DetailLevelBox = wxStaticBoxSizer:new(?wxHORIZONTAL, 
					  Panel,
					  [{label, "Detail level"}]),
    DetailLevel = wxSlider:new(Panel, ?wxID_ANY,
			       S#state.detail_level,
			       ?detail_level_min,
			       ?detail_level_max,
			       [{style, ?wxSL_LABELS},
				{size, {200,-1}}]),
    wxStatusBar:setStatusText(StatusBar, where_text(S)),
    wxFrame:connect(Frame, command_slider_updated),
    _ = wxSizer:add(DetailLevelBox, DetailLevel),
    _ = wxSizer:add(OptSizer, DetailLevelBox, [{border, 10}, {flag, ?wxALL}]),
    _ = wxSizer:addStretchSpacer(OptSizer),
    _ = wxSizer:add(MainSizer, OptSizer),
    _ = wxSizer:add(MainSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]),
                    [{flag, ?wxEXPAND}]),

    CanvasSizer = wxBoxSizer:new(?wxHORIZONTAL),
    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    {CanvasW,CanvasH} = wxPanel:getSize(Canvas),
    ScrollBar = wxScrollBar:new(Panel, ?wxID_ANY, [{style, ?wxSB_VERTICAL}]),

    _ = wxSizer:add(CanvasSizer, Canvas, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxSizer:add(CanvasSizer, ScrollBar, [{flag, ?wxEXPAND}]),
    _ = wxSizer:add(MainSizer, CanvasSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, left_up),
    wxPanel:connect(Canvas, right_up),
    wxPanel:connect(Canvas, size),
    Self = self(),
    wxPanel:connect(Canvas, paint, [{callback,  %% Needed on windows
				     fun(Ev, _) -> 
					     DC = wxPaintDC:new(Canvas),
					     wxPaintDC:destroy(DC),
					     Self ! Ev
				     end}]),
    wxPanel:connect(Canvas, key_down),
    wxPanel:connect(Canvas, enter_window, [{skip, true}]), 
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(ScrollBar, scroll_changed),
    wxPanel:setSize(Panel, {S#state.width, S#state.height}),
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:show(Frame),
    wxPanel:setFocus(Canvas),
    wxPanel:connect(Canvas, mousewheel),

    S3 = S2#state{title = Title,
		  frame = Frame, packer = Panel,
		  normal_font = NormalFont, bold_font = BoldFont,
		  canvas_width = CanvasW, canvas_height = CanvasH,
		  canvas = Canvas,
		  canvas_sizer = CanvasSizer,
		  scroll_bar = ScrollBar,
		  y_pos = ?initial_y * S#state.scale,
		  pen = wxPen:new(),
		  brush = wxBrush:new(),
		  print_d = undefined,
		  print_psdd = undefined,
		  menu_data = MenuData,
		  checkbox_data = CheckBoxData,
		  hide_actions_box = HideActions,
		  hide_actors_box = HideActors,
		  status_bar = StatusBar},
    DC = wxClientDC:new(Canvas),
    S4 = draw_all_actors(S3, DC),
    wxClientDC:destroy(DC),
    S4.

where_text(#state{n_events = N} = S) ->
    Pos = event_pos(S),
    lists:concat([Pos, " (", N, ")"]).

event_pos(#state{first_event = E, events = Events, n_events = Last}) ->
    case E of
	#e{pos = Pos} ->
	    Pos;
	first ->
	    case queue_length(Events) of
		0 ->
		    0;
		_ ->
		    1
	    end;
	last ->
	    Last
    end.

init_printers(#state{print_d = undefined, print_psdd = undefined} = S) ->
    PD = wxPrintData:new(),
    PSDD = wxPageSetupDialogData:new(PD),
    wxPrintData:setPaperId(PD, ?wxPAPER_A4),
    wxPageSetupDialogData:setMarginTopLeft(PSDD, {15,15}),
    wxPageSetupDialogData:setMarginBottomRight(PSDD, {15,15}),
    S#state{print_d = PD, print_psdd = PSDD};
init_printers(#state{} = S) ->
    S.

select_fonts(Scale) when is_integer(Scale) ->
    Size =
	case Scale of
	    1 -> 5;
	    2 -> 10;
	    3 -> 14;
	    4 -> 20;
	    S -> S*6
	end,
    {wxFont:new(Size, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
     wxFont:new(Size, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxBOLD,[])}.
	
get_value(Key, Pos, TupleList) when is_list(TupleList)->
    case lists:keysearch(Key, 1, TupleList) of
	{value, Tuple} when is_tuple(Tuple)->
	    element(Pos, Tuple);
	false ->
	    false
    end.

menuitem(Menu, Id, Text, UserData) ->
    Item = wxMenu:append(Menu, Id, Text),
    {wxMenuItem:getId(Item), Item, UserData}.

create_file_menu(Bar) ->
    Menu = wxMenu:new([]),
    Data = [menuitem(Menu, ?wxID_ANY, "Clear all events in the Collector", clear_all),
	    menuitem(Menu, ?wxID_ANY, "Load events to the Collector from file", load_all),
	    menuitem(Menu, ?wxID_ANY, "Save all events in the Collector to file", save_all),

	    menuitem(Menu, ?wxID_PRINT_SETUP, "Print setup", print_setup),
	    menuitem(Menu, ?wxID_ANY, "Print current page", print_one_page),
	    menuitem(Menu, ?wxID_PRINT, "Print all pages", print_all_pages),

	    menuitem(Menu, ?wxID_ANY, "Close this Viewer", close),
	    menuitem(Menu, ?wxID_ANY, "Close all other Viewers, but this (c)", close_all_others),
	    menuitem(Menu, ?wxID_ANY, "Close all Viewers and the Collector)   (C) ", close_all)],
    _ = wxMenu:insertSeparator(Menu, 3),
    _ = wxMenu:insertSeparator(Menu, 7),
    wxMenuBar:append(Bar, Menu, "File"),
    Data.

create_viewer_menu(Bar) ->
    Menu   = wxMenu:new([]),
    _ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Scroll this Viewer"),
                          [{enable, false}]),
    _ = wxMenu:appendSeparator(Menu),
    D1 = [menuitem(Menu, ?wxID_ANY, "First    (f)", first),
	  menuitem(Menu, ?wxID_ANY, "Last     (l)", last),
	  menuitem(Menu, ?wxID_ANY, "Prev     (p)", prev),
	  menuitem(Menu, ?wxID_ANY, "Next     (n)", next),
	  menuitem(Menu, ?wxID_ANY, "Refresh  (r)", refresh)],
    _ = wxMenu:appendSeparator(Menu),
    D2 = [menuitem(Menu, ?wxID_ANY, "Up   5   (Up)", up),
	  menuitem(Menu, ?wxID_ANY, "Down 5   (Down)", down)],
    _ = wxMenu:appendSeparator(Menu),
    _ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Actor visibility in this Viewer"),
                          [{enable, false}]),
    _ = wxMenu:appendSeparator(Menu),
    D3 = [menuitem(Menu, ?wxID_ANY, "Display all actors (a)", display_all)],
    _ = wxMenuBar:append(Bar, Menu, "Viewer"),
    [D1,D2,D3].

create_collector_menu(Bar) ->
    Menu = wxMenu:new([]),
    _ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Scroll all Viewers"),
                          [{enable, false}]),
    _ = wxMenu:appendSeparator(Menu),
    Data = [menuitem(Menu, ?wxID_ANY, "First   (F)", first_all),
	    menuitem(Menu, ?wxID_ANY, "Last    (L)", last_all),
	    menuitem(Menu, ?wxID_ANY, "Prev    (P)", prev_all),
	    menuitem(Menu, ?wxID_ANY, "Next    (N)", next_all),
	    menuitem(Menu, ?wxID_ANY, "Refresh (R)", refresh_all)],
    _ = wxMenuBar:append(Bar, Menu, "Collector"),
    Data.

create_filter_menu(S=#state{filter_menu = {Menu,Data}}, ActiveFilterName, Filters) ->
     wx:foreach(fun({_,I,_}) ->
			wxMenu:delete(Menu,I);
		   (I) ->
			try
			    wxMenu:delete(Menu,I)
			catch
			    _:Reason ->
				io:format("Could not delete item: ~tp, because ~tp.\n", [I, Reason])
			end
		end, 
		Data),
     Item = fun(F, {N, Acc}) when F#filter.name =:= all ->
		    Label = lists:concat([pad_string(F#filter.name, 20), "(0)"]),
		    {N+1, [menuitem(Menu, ?wxID_ANY, Label, {data, F})|Acc]};
	       (F, {N, Acc}) ->
		    Label = lists:concat([pad_string(F#filter.name, 20), "(", N, ")"]),
		    {N+1, [menuitem(Menu, ?wxID_ANY, Label, {data, F})|Acc]}
	    end,
     D1 = [I1 = wxMenu:append(Menu, ?wxID_ANY, "Same Filter New Scale"),
	   wxMenu:appendSeparator(Menu)],
     wxMenuItem:enable(I1, [{enable,false}]),
     {value, Filter} = lists:keysearch(ActiveFilterName, #filter.name, Filters),
     Same    = lists:concat([pad_string(ActiveFilterName, 20), "(=) same    scale"]),
     Larger  = lists:concat([pad_string(ActiveFilterName, 20), "(+) bigger  scale"]),
     Smaller = lists:concat([pad_string(ActiveFilterName, 20), "(-) smaller scale"]),
     D2 = [menuitem(Menu, ?wxID_ANY, Same, {data, Filter, 0}),
	   menuitem(Menu, ?wxID_ANY, Smaller, {data, Filter, -1}),
	   menuitem(Menu, ?wxID_ANY, Larger, {data, Filter, 1}),
	   wxMenu:appendSeparator(Menu),
	   I2 = wxMenu:append(Menu, ?wxID_ANY, "New Filter Same Scale"),
	   wxMenu:appendSeparator(Menu)],
     _ = wxMenuItem:enable(I2, [{enable,false}]),
     {_,D3} = lists:foldl(Item, {1,[]}, Filters),
     S#state{filter_menu = {Menu, lists:flatten([D1,D2,D3])}}.

create_help_menu(Bar) ->
    Menu = wxMenu:new([]),
    _ = menuitem(Menu, ?wxID_HELP, "Info", help),
    wxMenuBar:append(Bar, Menu, "Help").

clear_canvas(S) ->
    DC = wxClientDC:new(S#state.canvas),
    wxDC:setBackground(DC, ?wxWHITE_BRUSH), %% Needed on mac
    wxDC:clear(DC),
    {CanvasW, CanvasH} = wxPanel:getSize(S#state.canvas),
    wxSizer:recalcSizes(S#state.canvas_sizer),
    S2 = S#state{refresh_needed = false,
		 y_pos          = ?initial_y * S#state.scale,
		 canvas_width   = CanvasW, 
		 canvas_height  = CanvasH,
		 events         = queue_new()},
    S3 = draw_all_actors(S2, DC),
    wxClientDC:destroy(DC),
    S3.

replace_events(S, []) ->
    S#state{first_event = first,
	    last_event = first, 
	    events = queue_new()};
replace_events(S, Events) ->
    Queue = lists:foldl(fun(E, Q) -> queue_in(E, Q) end, queue_new(), Events),
    S#state{events = Queue}.

refresh_main_window(S) ->
    wx:batch(fun() ->
		     S2 = clear_canvas(S),
		     S3 = update_scroll_bar(S2),
		    display_events(S3, queue_to_list(S#state.events))
	     end).

display_events(S, []) ->
    S;
display_events(S, Events) ->
    DC = wxClientDC:new(S#state.canvas),
    S2 = lists:foldl(fun(E, State) -> display_event(E, State, DC) end, S, Events),
    wxClientDC:destroy(DC),
    S2.

collect_more_events(S, PrevKey = first, Try) ->
    PrevE = #e{event = undefined, key = PrevKey, pos = 0},
    S2 = S#state{first_event = PrevE, last_event = PrevE},
    do_collect_more_events(S2, Try, PrevE, []);
collect_more_events(S, PrevKey = last, Try) ->
    PrevE = #e{event = undefined, key = PrevKey, pos = S#state.n_events},
    S2 = S#state{first_event = PrevE, last_event = PrevE},
    do_collect_more_events(S2, Try, PrevE, []);
collect_more_events(S, #e{} = PrevE, Try) ->
    do_collect_more_events(S, Try, PrevE, []).

do_collect_more_events(#state{collector_pid = Collector,
			      event_order = Order, 
			      active_filter = Active,
			      filters = Filters} = S,
		       Try, 
		       PrevE,
		       Acc) ->
    Incr = 
	if
	    Try < 0 -> -1;
	    true    ->  1
	end,
    PrevKey = PrevE#e.key,
    {value, #filter{function = FilterFun}} =
	lists:keysearch(Active, #filter.name, Filters),
    {_S, _Incr, _Order, _Active, _FilterFun, LastE, NewEvents} =
	et_collector:iterate(Collector,
			     PrevKey, 
			     Try,
			     fun collect_event/2,
			     {S, Incr, Order, Active, FilterFun, PrevE, []}),
    Expected = abs(Try),
    Actual = length(NewEvents),
    Missing = Expected - Actual,
    {S2, Acc2, Try2} =
	if
	    Try < 0 ->
		{S#state{first_event = LastE}, NewEvents ++ Acc, -Missing};
	    true ->
		TmpEvents = lists:reverse(NewEvents),
		{S#state{last_event = LastE}, Acc ++ TmpEvents, Missing}
	end,
    if
	Missing =/= 0, PrevKey =/= LastE#e.key ->
	    do_collect_more_events(S2, Try2, LastE, Acc2);
	true ->  
	    {S2, Acc2}
    end.

collect_event(Event, {S, Incr, Order, Active, FilterFun, #e{pos = PrevPos}, Events}) ->
    Key = et_collector:make_key(Order, Event),
    E = #e{event = Event, key = Key, pos = PrevPos + Incr},
    {LastE, Events2} =
	case catch FilterFun(Event) of
	    true ->
		case is_hidden(Event#event.from, Event#event.to, S) of
		    true ->
			{E, Events};
		    false ->
			{E, [E | Events]}
		end;
	    {true, Event2} -> 
		Key2 = et_collector:make_key(Order, Event2),
		E2 = E#e{event = Event2, key = Key2},
		case is_hidden(Event2#event.from, Event2#event.to, S) of
		    true ->
			{E2, Events};
		    false ->
			{E2, [E2 | Events]}
		end;
	    false ->
		{E, Events};
	    Bad ->
		Contents = {bad_filter, S#state.active_filter, Bad, Event},
		Event2 = Event#event{contents = Contents,
				     from     = bad_filter,
				     to       = bad_filter},
		E2 = E#e{event = Event2},
		{E2, [E2 | Events]}
	end,
    {S, Incr, Order, Active, FilterFun, LastE, Events2}.

display_event(#e{event = Event} = E, S, DC)
  when Event#event.detail_level =< S#state.detail_level ->
    {FromRefresh, From} = ensure_actor(Event#event.from, S, DC),
    {FromName, FromPos, S2} = From,
    {ToRefresh, To} = ensure_actor(Event#event.to, S2, DC),
    {ToName, ToPos, S3} = To,
    S4 =
	if
	    FromRefresh =/= false, ToRefresh =/= false ->
		S3#state{refresh_needed = true,
			 events = queue_in(E, S3#state.events)};
	    FromName =:= ToName ->
		case S#state.hide_actions of
		    true -> 
			S3;
		    false -> 
			Label = name_to_string(Event#event.label),
			draw_named_arrow(Label, FromName, ToName, FromPos, ToPos, E, S3, DC)
		end;
	    true ->
		Label = name_to_string(Event#event.label),
		draw_named_arrow(Label, FromName, ToName, FromPos, ToPos, E, S3, DC)
	end,
    S4;
display_event(#e{}, S, _DC) ->
    S.

draw_named_arrow(Label, FromName, ToName, FromPos, ToPos, E, S, DC) ->
    case S#state.y_pos + (?incr_y *  S#state.scale) of
	_ when S#state.hide_actors =:= true, FromName =:= ?unknown ->
	    S;
	_ when S#state.hide_actors =:= true, ToName =:= ?unknown ->
	    S;
	Y when  Y > S#state.canvas_height ->
	    S#state{refresh_needed = true,
		    events = queue_in(E, S#state.events)};
	Y ->
	    S2 = S#state{y_pos = Y, events = queue_in(E, S#state.events)},
       %%here can start the async behaviour of the graphic chart
        case S2#state.async of
            true->
                draw_lifeline(S2,?initial_y*S2#state.scale,getFirstActorPos(S2),DC),%totally mark the "root" process
                case maps:get(E#e.event#event.label,S2#state.async_patterns) of%based on asynchronous patterns and type of event, I get his "counterpart"
                    spawn ->
                        S3 = draw_arrow(FromPos, ToPos, S2, DC),
                        Pos=getActorToPos(S3#state.actors,E,0),
                        draw_lifeline(S3,S3#state.y_pos,Pos,DC),
                        drawStartPoint(S3,DC,FromPos,S3#state.y_pos),
                        draw_label(Label, FromName, ToName, FromPos, ToPos, S3, DC);
                    'receive'->
                        S3=draw_arrow(FromPos,FromPos,S2,DC),
                        MirrorSendPos=cercaSpec(E#e.event,queue_to_list(S3#state.events),S3),
                        case MirrorSendPos==null of
                            false->
                                FromPosY=Y-((E#e.pos-MirrorSendPos)*?incr_y*S3#state.scale),
                                drawTest(E#e.event#event.contents,FromPos,ToPos,FromPosY,S3,DC),
                                S4=draw_arrow_async(FromPos,FromPosY,ToPos, S3, DC),
                                delete_label("send",ToPos,ToPos,FromPosY, S4, DC);
                            true->%handles the case in which there is a receive before a send !! (it should never occur)
                                draw_label(Label, FromName, ToName, FromPos, FromPos, S3, DC)
                        end;
                    send->
                        S3=draw_arrow(FromPos,FromPos,S2,DC),
                        drawStartPoint(S3,DC,FromPos,S3#state.y_pos),
                        draw_label(Label, FromName, ToName, FromPos, FromPos, S3, DC);
                    exit->
                        Pos=getActorToPos(S2#state.actors,E,0),
                        draw_deadline(S2,S2#state.y_pos,Pos,DC),
                        drawStartPoint(S2,DC,FromPos,S2#state.y_pos),
                        draw_label(Label, FromName, ToName, FromPos, ToPos, S2, DC)
                end;
            false->
            %%would be the non-use of asynchronous behavior and the use of "normal" behavior
	           S3 = draw_arrow(FromPos, ToPos, S2, DC),
	           draw_label(Label, FromName, ToName, FromPos, ToPos, S3, DC)
        end
    end.
%%FUNCTIONS OF ASYNC BEHAVIOUR OF GRAPHIC CHART%%%%%%%%

getMediumPoint({Xa,Ya},{Xb,Yb})->
    Xm=round((Xa+Xb)/2),
    Ym=round((Ya+Yb)/2),
    {Xm,Ym}.

dist2Points({Xa,Ya},{Xb,Yb})->
    round(math:sqrt(math:pow((Xb-Xa),2)+math:pow((Yb-Ya),2))).


normalize(Text,MedPoint,SecondMed,SupPoint,TextLen,CharWidth)->
    MedBetweenMedSup=getMediumPoint(MedPoint,SupPoint),
    Delta=round((TextLen-dist2Points(SecondMed,MedBetweenMedSup))/CharWidth),
    {Mess,NumMess}=lists:split(length(Text)-3,Text),
    FinalLen=round((TextLen/CharWidth)-Delta),
    NewText=string:slice(Mess,0,FinalLen-3),
    NewText++"..."++NumMess.

calcTextPoints(Text,Len,MedPoint,InfPoint,SupPoint,CharWidth)->
    SecondMed=getMediumPoint(MedPoint,InfPoint),
    LenToSup=dist2Points(MedPoint,SecondMed),
    LenBetweenMeds=dist2Points(SecondMed,SupPoint),
    if 
        Len=<LenToSup->
                calcTextPoints(Text,Len,MedPoint,SecondMed,SupPoint,CharWidth);
        LenBetweenMeds =< Len->
                {normalize(Text,MedPoint,SecondMed,SupPoint,Len,CharWidth),SecondMed};
        true->
            {Text,SecondMed}
    end.
   
drawTest(Text,FromPos,ToPos,FromPosY,S,DC)->
    io:fwrite("FOREGROUND COLOR: ~p~n",[wxDC:getTextForeground(DC)]),
    Const=6*S#state.scale,
    CharWidth=wxDC:getCharWidth(DC),
    TextLen=length(Text)*CharWidth,
    case FromPos<ToPos of%check if the send is more right than receive
        true->
        %This piece of code,obtain the two point of the parallel rect to the async arrow(to stay a little bit up from the arrow!!!)
        %and calculate IT medium point
            ParallelSendPoint={ToPos-Const,FromPosY-Const},
            ParallelReceivePoint={FromPos-Const,S#state.y_pos-Const},
            MedStart=getMediumPoint(ParallelReceivePoint,ParallelSendPoint),
            {NewText,{TextX,TextY}}=calcTextPoints(Text,TextLen,MedStart,ParallelReceivePoint,ParallelSendPoint,CharWidth),
        %
            Radians = calc_angle({FromPos, FromPosY}, {ToPos,S#state.y_pos});%Point A=Send Point B=Receive
        false->
            ParallelSendPoint={ToPos+Const,FromPosY-Const},
            ParallelReceivePoint={FromPos+Const,S#state.y_pos-Const},
            MedStart=getMediumPoint(ParallelReceivePoint,ParallelSendPoint),
            {NewText,{TextX,TextY}}=calcTextPoints(Text,TextLen,MedStart,ParallelSendPoint,ParallelReceivePoint,CharWidth),
            Radians=calc_angle({ToPos,S#state.y_pos},{FromPos, FromPosY})%Point A=Receive Point B=Send
    end,
    Angle=Radians*180/3.14,%obtain value angle in degree
    wxDC:setFont(DC,S#state.normal_font),
    wxDC:setTextForeground(DC,?wxBLUE),
    wxDC:drawRotatedText(DC,NewText,{TextX,TextY},Angle),
    S.

delete_label(Label,FromPos, ToPos,PosY, S, DC) ->
    Color= ?wxWHITE,
    Scale = S#state.scale,
    X = lists:min([FromPos, ToPos]) + (6 * Scale),
    Y = PosY,
    write_text(Label, X, Y, Color, S#state.normal_font, S, DC),%%
    write_text(Label, X, Y, Color, S#state.normal_font, S, DC),% I use it like a text cleaner
    write_text(Label, X, Y, Color, S#state.normal_font, S, DC),%%
    write_text(Label, X, Y, Color, S#state.normal_font, S, DC),%%
    write_text(Label, X, Y, Color, S#state.normal_font, S, DC),%%
    S.

getAsyncPatternsKeys(AsyncPattern)->
    case AsyncPattern of
        undefined->
            undefined;
        Map->
            L=maps:to_list(AsyncPattern),
            {Spawn,spawn}=lists:keyfind(spawn,2,L),
            {Send,send}=lists:keyfind(send,2,L),
            {Receive,'receive'}=lists:keyfind('receive',2,L),
            {Exit,exit}=lists:keyfind(exit,2,L),
            {Spawn,Send,Receive,Exit}
    end.

 keepAlive(S,EventSpawn,DC)->%used when scrolling,to keep alive already alive actors
    {FromPos,ToPos}={getActorFromPos(S#state.actors,EventSpawn,0),getActorToPos(S#state.actors,EventSpawn,0)},
    draw_lifeline(S,?initial_y,FromPos,DC),
    draw_lifeline(S,?initial_y,ToPos,DC).

getFirstActorPos(S)->
    FirstEvent=hd(queue_to_list(S#state.events)),
    getActorFromPos(S#state.actors,FirstEvent,0).

drawStartPoint(S,DC,FromPos,Y)->%%draw the start's point of a arrow in the chart
    wxPen:setColour(S#state.pen, ?wxBLACK),
    wxBrush:setColour(S#state.brush,?wxBLACK),
    wxDC:setPen(DC, S#state.pen),
    wxDC:setBrush(DC, S#state.brush),
    wxDC:drawCircle(DC,{FromPos+2,Y},2).

getActorFromPos([],_,Acc)->Acc;%given the list of actors and an event and,returns the position of the SENDER(from) actor of the event in the list
getActorFromPos([H|T],E,Acc)->
    case E#e.event#event.from==H#actor.name of
        true->
            Acc;
        false->
            getActorFromPos(T,E,Acc+1)
    end.

getActorToPos([],_,Acc)->Acc;%given the list of actors and an event and,returns the position of the RECEIVER(to) actor of the event in the list
getActorToPos([H|T],E,Acc)->
    case E#e.event#event.to==H#actor.name of
        true->
            Acc;
        false->
            getActorToPos(T,E,Acc+1)
    end.

draw_deadline(S,LineTopY,NumActor,DC)->% draws an actor's lifeless red line
    LineX=(?initial_x+(NumActor*?incr_x))*S#state.scale,%computes the position on the x-axis, based on the position of the actor in the list of actors
    %LineTopY=round((S#state.y_pos*S#state.scale)+((?incr_y/2)*S#state.scale)),
    LineBotY=S#state.canvas_height,
    wxPen:setColour(S#state.pen, ?wxWHITE),
    wxPen:setWidth(S#state.pen,3),
    wxDC:setPen(DC, S#state.pen),
    wxDC:drawLines(DC, [{LineX, LineTopY}, {LineX, LineBotY}]),
    wxPen:setWidth(S#state.pen,1),
    wxPen:setColour(S#state.pen, ?wxRED),
    wxDC:setPen(DC, S#state.pen),
    wxDC:drawLines(DC, [{LineX, LineTopY}, {LineX, LineBotY}]).

draw_lifeline(S,LineTopY,NumActor,DC)->%draws the big green line of life of an actor
    LineX=(?initial_x+(NumActor*?incr_x))*S#state.scale,%computes the position on the x-axis, based on the position of the actor in the list of actors
    %LineTopY=round((S#state.y_pos*S#state.scale)+((?incr_y/2)*S#state.scale)),
    LineBotY=S#state.canvas_height,%bottom line of the y axis
    wxPen:setColour(S#state.pen, ?wxGREEN),
    wxPen:setWidth(S#state.pen,3),
    wxDC:setPen(DC, S#state.pen),
    wxDC:drawLines(DC, [{LineX, LineTopY}, {LineX, LineBotY}]),
    wxPen:setWidth(S#state.pen,1).

 %%Algorithm looks for mirror events
cercaSpec(Event,Events,S)->%Taken as input the receive event and the queue of all events, look for the mirror send!
    %% ATTENTION, I CAN HAVE MORE EQUAL SENDS
    %Creates the list of event (s) mirroring to the receive in question
    Send=maps:get(send,S#state.async_patterns),%the send label according to the asynchronous communication pattern
    Receive=Event#event.label,
    %% Create the mirror sends list
    EventsSpec=[{Pos,Key,Spec}||{e,Pos,Key,Spec}<-Events,Spec#event.label==Send,Event#event.from==Spec#event.to,
                Event#event.to==Spec#event.from,Event#event.contents==Spec#event.contents],
    %% Create the mirror receives list
    SameEvents=[{Pos,Key,Same}||{e,Pos,Key,Same}<-Events,Same#event.label==Receive,Event#event.from==Same#event.from,
                Event#event.to==Same#event.to,Event#event.contents==Same#event.contents],
    %I get what number of receive is based on the time id of the event (trace_ts)
    Pos=getReceivePos(SameEvents,Event,1),%%lists start at index 1 not 0!
    %I'm going to get the mirror to the receive one !!
    case Pos=<length(EventsSpec) of
        true->element(1,lists:nth(Pos,EventsSpec));%I'm going to take the Pos-th element of the specular send and return the position!
        false->null
    end.

getReceivePos([],_,Acc)->Acc;%similar to getActorPos
getReceivePos([H|T],E,Acc)->
    case E#event.trace_ts==(element(3,H))#event.trace_ts of
        true->
            Acc;
        false->
            getReceivePos(T,E,Acc+1)
    end.

draw_arrow_async(FromPos,FromPosY,ToPos, S, DC)->%%draw the diagonal arrow
    Y = S#state.y_pos,
    wxPen:setColour(S#state.pen, ?wxBLACK),
    wxDC:setPen(DC, S#state.pen),
    wxDC:setBrush(DC, S#state.brush),
    case FromPos==ToPos of
        true->
            {Xbar,Ybar}=computeBaricenter(FromPos,Y,ToPos,FromPosY),
            wxDC:drawLines(DC, [{FromPos , Y},{Xbar,Ybar},{Xbar,Ybar},{ToPos, FromPosY}]);
        false->
            wxDC:drawLine(DC, {FromPos , Y},{ToPos, FromPosY})
    end,
    %% Draw arrow head
    %Radians = calc_angle({FromPos, Y}, {ToPos,FromPosY}),
    Radians = calc_angle({ToPos,FromPosY},{FromPos, Y}),
    Len = 5,
    Radians2 = Radians + 3.665191429188092,
    Radians3 = Radians + 2.617993877991494,
    {X3, Y3} = calc_point({FromPos, Y}, Len, Radians2),
    {X4, Y4} = calc_point({FromPos, Y}, Len, Radians3),
    Points = [{round(FromPos), round(Y)},
          {round(X3), round(Y3)},
          {round(X4), round(Y4)}],
    wxDC:setBrush(DC, S#state.brush),
    wxDC:drawPolygon(DC, Points, []),
    S.

computeBaricenter(FromPos,Y,ToPos,FromPosY)->%computes the center of gravity of a fictitious triangle, it is used to draw the broken arrows
    case FromPos==ToPos of
        false->
            Xbar=round((FromPos+ToPos+ToPos)/3),
            Ybar=round((Y+FromPosY+Y)/3),
            {Xbar,Ybar};
        true->
            Xbar=ToPos-10,
            Ybar=FromPosY+round((Y-FromPosY)/2),
            {Xbar,Ybar}
    end.

%%END ASYNCHRONOUS FUNCTIONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw_arrow(Pos, Pos, S, _DC) ->
    S;
draw_arrow(FromPos, ToPos, S, DC) ->
    Y = S#state.y_pos,
    wxPen:setColour(S#state.pen, ?wxBLACK),
    wxDC:setPen(DC, S#state.pen),
    wxDC:drawLine(DC, {FromPos , Y}, {ToPos, Y}),

    %% Draw arrow head
    Radians = calc_angle({FromPos, Y}, {ToPos, Y}),
    Len = 5,
    Radians2 = Radians + 3.665191429188092,
    Radians3 = Radians + 2.617993877991494,
    {X3, Y3} = calc_point({ToPos, Y}, Len, Radians2),
    {X4, Y4} = calc_point({ToPos, Y}, Len, Radians3),
    Points = [{round(ToPos), round(Y)},
	      {round(X3), round(Y3)},
	      {round(X4), round(Y4)}],
    wxBrush:setColour(S#state.brush, ?wxBLACK),
    wxDC:setBrush(DC, S#state.brush),
    wxDC:drawPolygon(DC, Points, []),
    S.

 %% Calculate angle in radians for a line between two points
calc_angle({X1, Y1}, {X2, Y2}) ->
    math:atan2((Y2 - Y1), (X2 - X1)).

 %% Calc new point at a given distance and angle from another point
calc_point({X, Y}, Length, Radians) ->
    X2 = round(X + Length * math:cos(Radians)),
    Y2 = round(Y + Length * math:sin(Radians)),
    {X2, Y2}.

draw_label(Label, FromName, ToName, FromPos, ToPos, S, DC) ->
    Color =
	if
	    FromName =:= ?unknown, 
	    ToName   =:= ?unknown -> {2,  71, 254};% blue
	    FromName =:= ?unknown -> {255,126,0};  % orange
	    ToName   =:= ?unknown -> {255,126,0};  % orange
	    FromPos  =:= ToPos    -> {2,  71, 254};% blue
	    true                  -> {227,38, 54}  % red
	end,
    Scale = S#state.scale,
    X = lists:min([FromPos, ToPos]) + (6 * Scale),
    Y = S#state.y_pos,
    write_text(Label, X, Y, Color, S#state.normal_font, S, DC),
    S.

draw_all_actors(S, DC) ->
    Scale = S#state.scale,
    Fun = fun(A, X) ->
		  case draw_actor(A, X, S, DC) of
		      true ->
			  X + (?incr_x * Scale);
		      false ->
			  X
		  end
	  end,
    lists:foldl(Fun, ?initial_x * Scale, S#state.actors),
    S.

%% Returns: {NeedsRefreshBool, {ActorPos, NewsS, NewActors}}
ensure_actor(Name, S, DC) ->
    do_ensure_actor(Name, S, S#state.actors, 0, DC).

do_ensure_actor(Name, S, [H | _], N, _DC) when H#actor.name =:= Name ->
    Pos = (?initial_x + (N * ?incr_x)) * S#state.scale,
    {false, {Name, Pos, S}};
do_ensure_actor(Name, S, [H | T], N, DC) ->
    if
    	S#state.hide_actors, H#actor.exclude ->
	    do_ensure_actor(Name, S, T, N, DC);
	true ->
	    do_ensure_actor(Name, S, T, N + 1, DC)
    end;
do_ensure_actor(Name, S, [], N, DC) ->
    %% A brand new actor, let's see if it does fit
    Pos = (?initial_x + (N * ?incr_x)) * S#state.scale,
    MaxActors = S#state.max_actors,
    if
	is_integer(MaxActors), N > MaxActors ->
	    %% Failed on max_actors limit, put into unknown
	    %% Assume that unknown always is in actor list
	    ensure_actor(?unknown, S, DC);
	Pos > (S#state.canvas_width - ((?initial_x - 15) * S#state.scale)) ->
	    %% New actor does not fit in canvas, refresh needed
	    A = create_actor(Name),
	    draw_actor(A, Pos, S, DC),
	    {true, {Name, Pos, S#state{actors = S#state.actors ++ [A]}}};
	true ->
	    %% New actor fits in canvas. Draw the new actor.
	    A = create_actor(Name),
	    draw_actor(A, Pos, S, DC),
	    {false, {Name, Pos, S#state{actors = S#state.actors ++ [A]}}}
    end.

draw_actor(A, LineX, S, DC) ->
    if
	S#state.hide_actors, A#actor.exclude ->
	    false;
	true ->
	    Scale = S#state.scale,
	    TextX = LineX - (5 * Scale),
	    {TextY, LineTopY, LineBotY} = calc_y(S),
	    Color =
		case A#actor.name of
		    ?unknown -> {255,126,0};% orange
		    _        -> {227,38,54} % red
		end,
	    {String, Font} =
		if
		    S#state.context =:= display, A#actor.exclude ->
			{"(" ++ A#actor.string ++ ")", S#state.normal_font};
		    S#state.context =:= display, A#actor.include ->
			{"[" ++ A#actor.string ++ "]", S#state.bold_font};
		    true  -> 
			{A#actor.string, S#state.normal_font}
		end,
	    write_text(String, TextX, TextY, Color, Font, S, DC),
        wxPen:setColour(S#state.pen, Color),
	    wxDC:setPen(DC, S#state.pen),
	    wxDC:drawLines(DC, [{LineX, LineTopY}, {LineX, LineBotY}]),
        %draw_lifeline(S,?initial_y*S#state.scale,1,DC),
	    true
    end.

calc_y(#state{canvas_height = Height, scale = Scale}) ->
    TextY    = ?initial_y * Scale,
    LineTopY = round(TextY + ((?incr_y  / 2) * Scale)),
    LineBotY = Height,
    %% LineBotY = round(Height - ((?incr_y / 2) * Scale)),
    {TextY, LineTopY, LineBotY}.

display_all(S) ->
    Actors = S#state.actors,
    Actors2 = [A#actor{include = false, exclude = false} || A <- Actors],
    S2 = S#state{actors = Actors2, 
		 display_all = true,
		 hide_actions = false,
		 hide_actors = false},
    wxCheckBox:setValue(S2#state.hide_actions_box, S2#state.hide_actions),
    wxCheckBox:setValue(S2#state.hide_actors_box, S2#state.hide_actors),
    revert_main_window(S2).

is_hidden(A, S) ->
    case S#state.display_all of
	true ->
	    A#actor.exclude;
	false ->
	    A#actor.exclude orelse not A#actor.include
    end.

is_hidden(From, To, S) ->
    Actors = S#state.actors,
    DisplayAll = S#state.display_all,
    FromMatch = lists:keysearch(From, #actor.name, Actors),
    ToMatch = lists:keysearch(To, #actor.name, Actors),
    case {FromMatch, ToMatch} of
	{false, false} ->
	    not DisplayAll;
	{false, {value, T}} ->
	    is_hidden(T, S);
	{{value, F}, false} ->
	    is_hidden(F, S);
	{{value, F}, {value, T}} when DisplayAll ->
	    is_hidden(F, S) orelse is_hidden(T, S);
	{{value, F}, {value, T}} when F#actor.include; T#actor.include ->
	    F#actor.exclude orelse T#actor.exclude;
	{{value, _F}, {value, _T}}->
	    true
    end.

move_actor(From, To, Actors, S) ->
    Pos      = #actor.name,
    ToName   = To#actor.name,
    FromName = From#actor.name,
    ToIx     = actor_index(ToName, Pos, Actors),
    FromIx   = actor_index(FromName, Pos, Actors),
    if
	FromIx =/= 0, ToIx =/= 0, ToIx > FromIx ->
	    Actors2 = lists:keydelete(FromName, Pos, Actors),
	    Actors3 = insert_actor_after(From, To, Actors2),
	    S2 = S#state{actors = Actors3},
	    refresh_main_window(S2);
	FromIx =/= 0, ToIx =/= 0 ->
	    Actors2 = lists:keydelete(FromName, Pos, Actors),
	    Actors3 = insert_actor_before(From, To, Actors2),
	    S2 = S#state{actors = Actors3},
	    refresh_main_window(S2);
	true ->
	    %% Ignore
	    S
    end.

insert_actor_after(From, To, [H | T]) ->
    case To#actor.name =:= H#actor.name of
	true  -> [H, From | T];
	false -> [H | insert_actor_after(From, To, T)]
    end;
insert_actor_after(_From, _To, []) ->
    [].

insert_actor_before(From, To, [H | T]) ->
    case To#actor.name =:= H#actor.name of
	true  -> [From, H | T];
	false -> [H | insert_actor_before(From, To, T)]
    end;
insert_actor_before(_From, _To, []) ->
    [].

actor_index(_Key, _Pos, []) ->
    0;
actor_index(Key, Pos, [H | T]) ->
    case Key =:= element(Pos, H) of
	false -> actor_index(Key, Pos, T) + 1;
	true  -> 1
    end.

y_to_n(Y, S) ->
    Y2   = ((Y / S#state.scale) - ?initial_y + (?incr_y / 2)),
    N    = round(Y2 / ?incr_y - 0.2),
    MaxN = queue_length(S#state.events),
    if
	N =< 0   -> actor;
	N > MaxN -> actor;
	true     -> {event, N}
    end.

x_to_n(X, S) ->
    Scale   = S#state.scale,
    Len = length(S#state.actors),
    X2 = X - (?initial_x * Scale),
    N  = X2 / (?incr_x * Scale),
    N2 = trunc(N + 1.5),
    if
	N2 > Len -> Len;
	N2 < 1   -> 1;
	true     -> N2
    end.

write_text(Text, X, Y, Color, Font, S, DC) ->
    wxDC:setFont(DC, Font),
    wxDC:setTextForeground(DC, Color),
    wxDC:drawText(DC, Text, {X, round(Y - (?incr_y * S#state.scale / 2))-3}).

do_open_event(S, N) ->
    Events = queue_to_list(S#state.events),
    S2 = S#state{events = list_to_queue(Events)},
    case catch lists:nth(N, Events) of
	{'EXIT', _} ->
	    {error, {no_such_event, N}};
	#e{key = Key} ->
	    Pid = S#state.collector_pid,
	    Fun = fun create_contents_window/2,
	    Prev = et_collector:iterate(Pid, Key, -1),
	    {S2, Res} = 
		if
		    Prev =:= Key ->
			et_collector:iterate(Pid, first, 1, Fun, {S2, []});
		    true ->
			et_collector:iterate(Pid, Prev, 1, Fun, {S2, []})
		end,
	    case Res of
		[] ->
		    {error, no_contents_viewer_started};
		[Single] ->
		    Single;
		Multi ->
		    {error, {too_many, Multi}}
	    end
    end.

create_contents_window(Event, {S, Res}) ->
    Options = [{viewer_pid, self()},
	       {event, Event},
	       {event_order, S#state.event_order},
	       {active_filter, S#state.active_filter},
	       {wx_debug, S#state.wx_debug}
	       | S#state.filters],
    case catch et_wx_contents_viewer:start_link(Options) of
	{ok, Pid} ->
	    {S, [{ok, Pid} | Res]};
	{error, Reason} ->
	    ok = error_logger:format("~p(~p): create_contents_window(~tp) ->~n     ~tp~n",
				     [?MODULE, self(), Options, Reason]),
	    {S, [{error, Reason} | Res]};
	Stuff ->
	    {S, [{error, {stuff, Stuff}} | Res]}
    end.

print_setup(S) ->
    S2 = #state{print_psdd = PSDD0, print_d = PD0} = init_printers(S),

    wxPageSetupDialogData:setPrintData(PSDD0, PD0),
    PSD = wxPageSetupDialog:new(S#state.frame, [{data,PSDD0}]),
    wxPageSetupDialog:showModal(PSD),

    PSDD1 = wxPageSetupDialog:getPageSetupData(PSD),
    PD1 = wxPageSetupDialogData:getPrintData(PSDD1),

    %% Create new objects using copy constructor
    PD = wxPrintData:new(PD1),
    PsDD = wxPageSetupDialogData:new(PSDD1),
    wxPageSetupDialog:destroy(PSD),
    wxPageSetupDialogData:destroy(PSDD0),
    wxPrintData:destroy(PD0),
    S2#state{print_psdd=PsDD, print_d=PD}.

print(#state{print_d = undefined, print_psdd = undefined} = S, Scope) ->
    S2 = print_setup(S),
    print(S2, Scope);
print(#state{print_psdd = PSDD, print_d = PD} = S, Scope) ->
    PDD = wxPrintDialogData:new(PD),
    wxPrintDialogData:enablePrintToFile(PDD, true),
    wxPrintDialogData:enablePageNumbers(PDD, true),
    wxPrintDialogData:enableSelection(PDD, true),
    Tab = ets:new(?MODULE, [public]),
    GetPageInfo =
	fun(This) ->
		{_, _, PW, PH} = wxPrintout:getPaperRectPixels(This),
		PrinterS = S#state{context = printer,
				   canvas_width = PW,
				   canvas_height = PH},
		EventsPerPage = events_per_page(PrinterS, PH),
		PagedEvents = paged_events(PrinterS, Scope, EventsPerPage),
		[ets:insert(Tab, PE) || PE <- PagedEvents],
		ets:insert(Tab, PrinterS),
		NumPages = length(PagedEvents),
		{1, NumPages, 1, NumPages} 
	end,
    HasPage = 
	fun(_This, Page) ->
		Size = ets:info(Tab, size),
		NumPages = Size - 1,
		(Page >= 1) andalso (Page =< NumPages) 
	end,
    OnPrintPage =
	fun(This, Page) ->
		wxPrintout:mapScreenSizeToPageMargins(This, PSDD),
		[PrinterS] = ets:lookup(Tab, state),
		Events = ets:lookup_element(Tab, Page, 2),
		DC = wxPrintout:getDC(This),
		PrinterS2 = draw_all_actors(PrinterS, DC),
		PrinterS3 = PrinterS2#state{y_pos = ?initial_y * PrinterS2#state.scale},
		lists:foldl(fun(E, State) -> display_event(E, State, DC) end, 
			    PrinterS3,
			    Events),
		true
	end,
    Printout1 = wxPrintout:new("Print", OnPrintPage,
			       [{getPageInfo, GetPageInfo}, {hasPage, HasPage}]),
    Printout2 = wxPrintout:new("Print", OnPrintPage,
			       [{getPageInfo, GetPageInfo}, {hasPage, HasPage}]),
    Preview = wxPrintPreview:new(Printout1, [{printoutForPrinting, Printout2}, {data,PDD}]), 
    case wxPrintPreview:isOk(Preview) of
	true ->
	    PF = wxPreviewFrame:new(Preview, S#state.frame, []),
	    wxPreviewFrame:centre(PF, [{dir, ?wxBOTH}]),
	    wxPreviewFrame:initialize(PF),
	    wxPreviewFrame:centre(PF),
	    wxPreviewFrame:show(PF),
	    OnClose = fun(_Wx, EventRef) -> ets:delete(Tab), wxEvent:skip(EventRef) end,
	    wxPreviewFrame:connect(PF, close_window, [{callback, OnClose}]);
	false ->
	    io:format("Could not create preview window.\n"
		      "Perhaps your current printer is not set correctly?~n", []),
	    wxPrintPreview:destroy(Preview),
	    ets:delete(Tab)
    end,
    S.

paged_events(S, Scope, EventsPerPage) ->
   {_, Events} =
	case Scope of
	    print_one_page ->
		revert(S#state{events_per_page = EventsPerPage});
	    print_all_pages ->
		collect_more_events(S, first, S#state.n_events)
	end,
    split_list(Events, EventsPerPage).

split_list(List, N) when is_integer(N), N > 0 ->
    do_split_list(List, N, 1, []).

do_split_list([], _N, _Page, Acc) ->
    lists:reverse(Acc);
do_split_list(List, N, Page, Acc) ->
    {Items, Rest} = pick_n(List, N, []),
    do_split_list(Rest, N, Page + 1, [{Page, Items} | Acc]).

get_latest_resize(#wx{obj = ObjRef, event = #wxSize{}} = Wx) ->
    receive
	#wx{obj = ObjRef, event = #wxSize{}} = Wx2 ->
	    get_latest_resize(Wx2)
    after 100 ->
	    Wx
    end.

get_latest_scroll(#wx{obj = ObjRef, event = #wxScroll{type = scroll_changed}} = Wx) ->
    receive
	#wx{obj = ObjRef, event = #wxScroll{type = scroll_changed}} = Wx2 ->
	    get_latest_scroll(Wx2)
    after 100 ->
	    Wx
    end.

update_scroll_bar(#state{scroll_bar      = ScrollBar,
			 status_bar      = StatusBar,
			 events_per_page = EventsPerPage,
			 n_events        = N} = S) ->
    Opts = [{refresh, true}],
    {_, LineTopY, LineBotY} = calc_y(S),
    Range = LineBotY - LineTopY,
    EventPos =
	case event_pos(S) of
	    1 -> 0;
	    P -> P
	end,
    if
	N =/= 0,
	EventsPerPage =/= 0 ->
	    PixelsPerEvent = Range / EventsPerPage,
	    Share = EventsPerPage / N,
	    wxScrollBar:setScrollbar(ScrollBar,
                                     trunc(EventPos * Share * PixelsPerEvent),
				     round(Share * Range),
				     Range,
				     round(Share * Range),
				     Opts);
	true ->
	    wxScrollBar:setScrollbar(ScrollBar,
				     0,
				     Range,
				     Range,
				     Range,
				     Opts)
    end,
    wxStatusBar:setStatusText(StatusBar, where_text(S)),
    S.

events_per_page(S, PageHeight) ->
    EventsPerPage = ((PageHeight - (?initial_y * S#state.scale)) div (?incr_y * S#state.scale)),
    lists:max([1, EventsPerPage]).	

select_file(Frame, Message, DefaultFile, Style) ->
    Dialog = wxFileDialog:new(Frame,
                              [{message, Message},
                               {defaultDir, filename:dirname(DefaultFile)},
                               {defaultFile, filename:basename(DefaultFile)},
                               {style, Style}]),
    Choice = 
        case wxMessageDialog:showModal(Dialog) of
            ?wxID_CANCEL ->  cancel;
            ?wxID_OK -> {ok, wxFileDialog:getPath(Dialog)}
        end,
    wxFileDialog:destroy(Dialog),
    Choice.

%%%----------------------------------------------------------------------
%%% String padding of actors
%%%----------------------------------------------------------------------

opt_create_actor(Name, Tag, S) ->
    Actors = S#state.actors,
    New =
	case lists:keysearch(Name, #actor.name, Actors) of
	    {value, Old} -> Old;
	    false        -> create_actor(Name)
	end,
    case Tag of
	include -> New#actor{include = true};
	exclude -> New#actor{exclude = true}
    end.

create_actor(Name) ->
    String = name_to_string(Name),
    %% PaddedString = pad_string(String, 8),
    #actor{name = Name, string = String, include = false, exclude = false}.

name_to_string(Name) ->
    case catch io_lib:format("~ts", [Name]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~tw", [Name]));
        GoodString  -> lists:flatten(GoodString)
    end.

pad_string(Atom, MinLen) when is_atom(Atom) ->
    pad_string(atom_to_list(Atom), MinLen);
pad_string(String, MinLen) when is_integer(MinLen), MinLen >= 0 ->
    Len = string:length(String),
    case Len >= MinLen of
        true ->
            String;
        false ->
            String ++ lists:duplicate(MinLen - Len, $ )
    end.

%%%----------------------------------------------------------------------
%%% Queue management
%%%----------------------------------------------------------------------

queue_new() ->
    {0, [], []}.

queue_in(X, {Size, In, Out}) ->
    {Size + 1, [X | In], Out}.

%% queue_out(Q) ->
%%     case Q of
%%         {Size, In, [H | Out]} -> {{value, H}, {Size - 1, In, Out}};
%%         {Size, [], []}        -> {empty, {Size, [], []}};
%%         {Size, In, _}         -> queue_out({Size, [], lists:reverse(In)})
%% end.

queue_to_list({_Size, [], Out}) ->
    Out;
queue_to_list({_Size, In, Out}) ->
    Out ++ lists:reverse(In).

queue_length({Size, _In, _Out}) ->
    Size.

list_to_queue(List) when is_list(List) ->
    {length(List), [], List}.
