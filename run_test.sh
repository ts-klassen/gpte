#!/bin/bash

rebar3 eunit &&
rebar3 ct --verbose &&
rebar3 cover &&
echo ALL_TEST_PASS
