#!/usr/bin/env python

import sys
import subunit
import testtools
import json
import datetime
import collections

import six
import ast

# Adapted from
# https://github.com/openstack/os-testr/blob/206e3275c16906975fd4a689c5a7f34aefbda671/os_testr/subunit_trace.py

# Push result to:
# OSTREAM = sys.stdout
OSTREAM = open('/tmp/the-trace.json', 'w')

# LOG prefix
# e.g., 2017-11-22 15:17:14,810 INFO ...
TODAY = datetime.date.today()
LOG_PREFIX = "%s-" % TODAY.year

# Do not extract sql for (sql extraction goes wrong):
BAN_TESTS = [
    # 'nova.tests.unit.db.test_db_api.FloatingIpTestCase.test_floating_ip_bulk_destroy',
    # 'nova.tests.unit.api.openstack.compute.test_floating_ips_bulk.FloatingIPBulkV21.test_create_ips',
    # 'nova.tests.unit.api.openstack.compute.test_floating_ips_bulk.FloatingIPBulkV21.test_list_ip_by_host',
    # 'nova.tests.unit.api.openstack.compute.test_floating_ips_bulk.FloatingIPBulkV21.test_list_ips',
    # 'nova.tests.unit.network.test_manager.FlatNetworkTestCase.test_validate_reserved',
    # 'nova.tests.unit.network.test_manager.FlatNetworkTestCase.test_validate_reserved_start_end',
    # 'nova.tests.unit.network.test_manager.VlanNetworkTestCase.test_vlan_multiple_with_dhcp_server',
    # 'nova.tests.unit.test_fixtures.TestOutputStream.test_output',
]

def split_lines(content):
    """Splits log into lines"""
    res = []

    if isinstance(content, six.string_types):
        res = content.splitlines()
    elif isinstance(content, testtools.content.Content):
        res = content.as_text().splitlines()

    return res

def filter_sql_query(log):
    """Extracts SQL statements from log"""
    sqls = []
    in_sql = False

    for q in log:
        # DEBUG:
        # print(q)

        # Check if its a new log line.
        #
        # FIXME: may omit some result if run around midnight at the
        # end of the year! But, honestly, from me to me, I'm the only
        # one you gonna use this, and do not expect me to work on New
        # Year's Eve. So, don't care in fixing it!
        is_new_log = q.startswith(LOG_PREFIX)
        if is_new_log and 'sqlalchemy.engine.base.Engine' in q:
            # Remove '2017-11-22 15:17:14,810 INFO [sqlalchemy.engine.base.Engine] '
            q = q[61:].strip()
            sqls.append(q)
            in_sql = True
        elif in_sql and not is_new_log:
            # Rest of previous SQL query: append to previous sql
            sqls[-1] = sqls[-1] + ' ' + q.strip()
        else:
            in_sql = False

    return sqls

def to_string(value):
    res = ""

    if isinstance(value, str):
        res = value.encode('utf-8')
    elif isinstance(value, unicode):
        res = value
    else:
        res = str(value)

    return res

def cope_sql_template(log):
    """Copes with templated SQL queries.

    There is different strategies available here. The strategy choice
    is hard-coded below:
    - forget: Remove parameters from the templated query.
    - pairwise: Put the query and its parameters in a pair.
    - expand: Expand the templated queries with parameters (not
      intensively tested).

    """
    def forget(sql, params):
        """Removes parameters from the templated query.

        Log
        > "SELECT a FROM b WHERE a.c = ? AND a.d = ?"
        > "(1,2)"
        Become
        > "SELECT a FROM b WHERE a.c = ? AND a.d = ?"
        """
        return sql

    def pairwise(sql, params):
        """Puts the query and its parameters in a pair.

        Log
        > "SELECT a FROM b WHERE a.c = ? AND a.d = ?"
        > "(1,2)"
        Become
        > ( "SELECT a FROM b WHERE a.c = ? AND a.d = ?"
        > , "(1,2)")
        """
        try:
            params = ast.literal_eval(params)
        except SyntaxError:
            params = ''.join(params) # Force string copy
        finally:
            return (sql, params)


    def expand(sql, params):
        """Expand SQL template.

        Expand
        > "SELECT a FROM b WHERE a.c = ? AND a.d = ?"
        > "(1,2)"
        Into
        > "SELECT a FROM b WHERE a.c = 1 AND a.d = 2"
        """
        # DEBUG:
        # print("sql: %s,\n values: %s" % (sql, params))
        params = ast.literal_eval(params)
        for p in params:
            sql = sql.replace('?', to_string(p), 1)

        return sql


    def is_template_params(params):
        res = False

        if isinstance(params, six.string_types):
            res = params.startswith('(') or params.startswith('{')

        return res

    # Expand template
    log.reverse()
    for i, l in enumerate(log):
        # This is template parameters
        if is_template_params(l):
            sql = log[i+1] # Templated query

            # Chose *between* `keep`, `pairwise` and `expand`
            # log[i+1] = keep(sql, l)
            log[i+1] = pairwise(sql, l)
            # log[i+1] = expand(sql, l)


    # Remove template values
    log.reverse()
    log[:] = [ sql for sql in log if not is_template_params(sql) ]

    return log

def formatsql(log):
    """Formats SQL statement frog log"""
    log = split_lines(log)
    log = filter_sql_query(log)
    log = cope_sql_template(log)

    return log

def getduration(timestamps):
    """Returns the total time in seconds to perform the test"""
    start, end = timestamps

    if not start or not end:
        duration = ''
    else:
        delta = end - start
        duration = delta.total_seconds()

    return duration

def handle_test(test):
    name = test['id']
    status = test['status'] is 'success'

    if status and name not in BAN_TESTS:
        # DEBUG:
        # print(name)
        duration = getduration(test['timestamps'])
        sql = formatsql(test['details'].get("pythonlogging:''", ''))
        res = {
            'name': name,
            'duration': duration,
            'sql': sql
        }
        OSTREAM.write(json.dumps(res))
        OSTREAM.write(',\n')

def trace():
    global OSTREAM
    case = subunit.ByteStreamToStreamResult(sys.stdin, non_subunit_name='stdout')
    result = testtools.StreamToDict(handle_test) # Call handle_test
                                                 # when test completes

    OSTREAM.write("[\n")
    result.startTestRun()
    case.run(result)
    result.stopTestRun()
    OSTREAM.write("]\n")
    OSTREAM.close()

exit(trace())
