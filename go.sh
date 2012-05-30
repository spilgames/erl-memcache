#!/bin/bash

erl -pa ebin -pa deps/*/ebin -config conf/memcache -hidden -setcookie spapi -s elibs_reloader -s memcache -name "memcache"
