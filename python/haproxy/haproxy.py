# -*- coding: utf-8 -*-
# This requires python3

from parsec import *

sp = spaces()                      # spaces
tk = lambda p: string(p) << sp     # token
num = regex(r'\d+').parsecmap(int) # number
opt = lambda p: times(p, 0, 1)     # optional
char = regex(r'.')                 # parses a single char -- regex :(

@generate
def captured_headers():
  '''Parses captured request/response headers'''
  # TODO: returns a list of header by matching on sep '|'
  heads = yield tk('{') >> many(none_of('}')).parsecmap(''.join) << tk('}')
  return heads

@generate
def httplog():
  '''Parses haproxy http log'''
  # https://cbonte.github.io/haproxy-dconv/1.6/configuration.html#8.2.3
  hap = {}

  # 10.0.1.2:33317
  hap['client_ip'] = yield regex(r'(\d+\.){3}\d+:\d+') << sp
  # [06/Feb/2009:12:14:14.655]
  hap['accept_date'] = yield tk('[') >> regex(r'.*?\.\d+') << tk(']') << sp
  # http-in
  hap['frontend_name'] = yield regex(r'\S+') << sp
  # static/srv1
  hap['backend_name'] = yield many(none_of('/')) << tk('/')
  hap['server_name'] = yield regex(r'\S+') << sp
  # 10/0/30/69/109
  hap['Tq'] = yield num << tk('/')
  hap['Tw'] = yield num << tk('/')
  hap['Tc'] = yield num << tk('/')
  hap['Tr'] = yield num << tk('/')
  hap['Tt'] = yield num << sp
  # 200
  hap['status_code'] = yield num << sp
  # 27500 (may be prefixed by a '+')
  hap['bytes_read'] = yield opt(tk('+')) >> num << sp
  # - - (captured_cookie ignored)
  yield tk('-') << sp << tk('-') << sp
  # ----
  hap['termination_state'] = yield times(char,4) << sp
  # 1/1/1/1/0
  hap['actconn'] = yield num << tk('/')
  hap['feconn'] = yield num << tk('/')
  hap['beconn'] = yield num << tk('/')
  hap['srv_conn'] = yield num << tk('/')
  hap['retries'] = yield opt(tk('+')) >> num << sp
  # 0/0
  hap['server_queue'] = yield num << tk('/')
  hap['backend_queue'] = yield num << sp
  # {1wt.eu}
  hap['captured_request_headers'] = yield opt(captured_headers)
  # {}
  hap['captured_response_headers'] = yield opt(captured_headers)
  # "GET /index.html HTTP/1.1"
  hap['http_request'] = yield\
    tk('"') >> many(none_of('"')).parsecmap(''.join) << tk('"')

  return hap

hap_httplog = (
  '10.0.1.2:33317 [06/Feb/2009:12:14:14.655] http-in '
  'static/srv1 10/0/30/69/109 200 2750 - - ---- 1/1/1/1/0 0/0 {1wt.eu} '
  '{} "GET /index.html HTTP/1.1"')

hap_httplog_noheaders = (
  '10.0.1.2:33317 [06/Feb/2009:12:14:14.655] http-in '
  'static/srv1 10/0/30/69/109 200 2750 - - ---- 1/1/1/1/0 0/0 '
  '"GET /index.html HTTP/1.1"')

print(httplog.parse(hap_httplog))
print(httplog.parse(hap_httplog_noheaders))
