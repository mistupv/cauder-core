%IMPORTANT
%%THIS FILE IS MODIFIED FROM et_viewer.erl OF OTP STANDARD LIBRARY FOR PARTIAL-ORDER GRAPHIC CHART
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

-module(et_viewer).
-export([file/1,
         start/0,
         start/1,
         start/2,
	 start_link/1,
	 start_link/2,
	 open_event/2,
	 stop/1, 
         get_collector_pid/1]).

-include_lib("et/include/et.hrl").
-include_lib("et/src/et_internal.hrl").


-define(unknown, "UNKNOWN").

file(FileName) ->
    start_link([{trace_client, {file, FileName}}], default).

start() ->
    start([{trace_global, true}], default).

start(GUI) when GUI =:= wx; GUI =:= default ->
    start_link([{trace_global, true}], GUI);
start(Options) ->
    start_link([{parent_pid, undefined} | Options], default).

start(Options, GUI) ->
    start_link([{parent_pid, undefined} | Options], GUI).

start_link(GUI) when GUI =:= wx; GUI =:= default ->
    start_link([{trace_global, true}], GUI);
start_link(Options) ->
    start_link(Options, default).

start_link(Options, GUI) -> 
    case GUI of
	wx ->
	    et_wx_viewer:start_link(Options);
	default ->
	    start_link(Options, which_gui())
    end.

which_gui() -> wx.

get_collector_pid(ViewerPid) ->
    call(ViewerPid, get_collector_pid).

stop(ViewerPid) ->
    call(ViewerPid, stop).

open_event(ViewerPid, N) ->
    call(ViewerPid, {open_event, N}).

call(ViewerPid, Request) ->
    gen_server:call(ViewerPid, Request, infinity).

