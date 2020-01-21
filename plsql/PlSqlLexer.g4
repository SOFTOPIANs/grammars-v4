/**
 * Oracle(c) PL/SQL 11g Parser
 *
 * Copyright (c) 2009-2011 Alexandre Porcelli <alexandre.porcelli@gmail.com>
 * Copyright (c) 2015-2019 Ivan Kochurkin (KvanTTT, kvanttt@gmail.com, Positive Technologies).
 * Copyright (c) 2017 Mark Adams <madams51703@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lexer grammar PlSqlLexer;

options {
    superClass=PlSqlLexerBase;
}

@lexer::postinclude {
#include <PlSqlLexerBase.h>
}

ABORT:                        'ABORT';
ACCESS:                       'ACCESS';
ACCESSED:                     'ACCESSED';
ACCOUNT:                      'ACCOUNT';
ACTIVATE:                     'ACTIVATE';
ADD:                          'ADD';
ADMIN:                        'ADMIN';
ADMINISTER:                   'ADMINISTER';
ADVISE:                       'ADVISE';
ADVISOR:                      'ADVISOR';
AFTER:                        'AFTER';
AGENT:                        'AGENT';
AGGREGATE:                    'AGGREGATE';
A_LETTER:                     'A';
ALL:                          'ALL';
ALLOCATE:                     'ALLOCATE';
ALLOW:                        'ALLOW';
ALTER:                        'ALTER';
ALWAYS:                       'ALWAYS';
ANALYZE:                      'ANALYZE';
AND:                          'AND';
ANY:                          'ANY';
ANYSCHEMA:                    'ANYSCHEMA';
APPLY:                        'APPLY';
ARCHIVE:                      'ARCHIVE';
ARCHIVELOG:                   'ARCHIVELOG';
ARRAY:                        'ARRAY';
AS:                           'AS';
ASC:                          'ASC';
ASSOCIATE:                    'ASSOCIATE';
ASYNCHRONOUS:                 'ASYNCHRONOUS';
AT:                           'AT';
ATTRIBUTE:                    'ATTRIBUTE';
ATTRIBUTES:                   'ATTRIBUTES';
AUDIT:                        'AUDIT';
AUTHENTICATED:                'AUTHENTICATED';
AUTHENTICATION:               'AUTHENTICATION';
AUTHID:                       'AUTHID';
AUTOALLOCATE:                 'AUTOALLOCATE';
AUTO:                         'AUTO';
AUTOEXTEND:                   'AUTOEXTEND';
AUTOMATIC:                    'AUTOMATIC';
AUTONOMOUS_TRANSACTION:       'AUTONOMOUS_TRANSACTION';
AVAILABILITY:                 'AVAILABILITY';
BACKUP:                       'BACKUP';
BASIC:                        'BASIC';
BASICFILE:                    'BASICFILE';
BATCH:                        'BATCH';
BECOME:                       'BECOME';
BEFORE:                       'BEFORE';
BEGIN:                        'BEGIN';
BETWEEN:                      'BETWEEN';
BFILE:                        'BFILE';
BIGFILE:                      'BIGFILE';
BINARY:                       'BINARY';
BINARY_DOUBLE:                'BINARY_DOUBLE';
BINARY_FLOAT:                 'BINARY_FLOAT';
BINARY_INTEGER:               'BINARY_INTEGER';
BITMAP:                       'BITMAP';
BLOB:                         'BLOB';
BLOCK:                        'BLOCK';
BLOCKSIZE:                    'BLOCKSIZE';
BODY:                         'BODY';
BOOLEAN:                      'BOOLEAN';
BOTH:                         'BOTH';
BREADTH:                      'BREADTH';
BUFFER_POOL:                  'BUFFER_POOL';
BUILD:                        'BUILD';
BULK:                         'BULK';
BY:                           'BY';
BYTE:                         'BYTE';
CACHE:                        'CACHE';
CALL:                         'CALL';
CANCEL:                       'CANCEL';
CANONICAL:                    'CANONICAL';
CASCADE:                      'CASCADE';
CASE:                         'CASE';
CAST:                         'CAST';
CERTIFICATE:                  'CERTIFICATE';
CHAINED:                      'CHAINED';
CHANGE:                       'CHANGE';
CHARACTER:                    'CHARACTER';
CHAR:                         'CHAR';
CHAR_CS:                      'CHAR_CS';
CHECK:                        'CHECK';
CHECKPOINT:                   'CHECKPOINT';
CHR:                          'CHR';
CHUNK:                        'CHUNK';
CLASS:                        'CLASS';
CLEAR:                        'CLEAR';
C_LETTER:                     'C';
CLOB:                         'CLOB';
CLONE:                        'CLONE';
CLOSE:                        'CLOSE';
CLUSTER:                      'CLUSTER';
COALESCE:                     'COALESCE';
COLLECT:                      'COLLECT';
COLUMN:                       'COLUMN';
COLUMNS:                      'COLUMNS';
COLUMN_VALUE:                 'COLUMN_VALUE';
COMMENT:                      'COMMENT';
COMMIT:                       'COMMIT';
COMMITTED:                    'COMMITTED';
COMPACT:                      'COMPACT';
COMPATIBILITY:                'COMPATIBILITY';
COMPILE:                      'COMPILE';
COMPLETE:                     'COMPLETE';
COMPOUND:                     'COMPOUND';
COMPRESS:                     'COMPRESS';
CONNECT_BY_ROOT:              'CONNECT_BY_ROOT';
CONNECT:                      'CONNECT';
CONSIDER:                     'CONSIDER';
CONSISTENT:                   'CONSISTENT';
CONSTANT:                     'CONSTANT';
CONSTRAINT:                   'CONSTRAINT';
CONSTRAINTS:                  'CONSTRAINTS';
CONSTRUCTOR:                  'CONSTRUCTOR';
CONTAINER:                    'CONTAINER';
CONTAINER_DATA:               'CONTAINER_DATA';
CONTENT:                      'CONTENT';
CONTEXT:                      'CONTEXT';
CONTINUE:                     'CONTINUE';
CONTROLFILE:                  'CONTROLFILE';
CONVERT:                      'CONVERT';
CORRUPTION:                   'CORRUPTION';
CORRUPT_XID_ALL:              'CORRUPT_XID_ALL';
CORRUPT_XID:                  'CORRUPT_XID';
COST:                         'COST';
COUNT:                        'COUNT';
CREATE:                       'CREATE';
CREATION:                     'CREATION';
CROSS:                        'CROSS';
CUBE:                         'CUBE';
CURRENT:                      'CURRENT';
CURRENT_USER:                 'CURRENT_USER';
CURSOR:                       'CURSOR';
CUSTOMDATUM:                  'CUSTOMDATUM';
CYCLE:                        'CYCLE';
DANGLING:                     'DANGLING';
DATABASE:                     'DATABASE';
DATA:                         'DATA';
DATAFILE:                     'DATAFILE';
DATAFILES:                    'DATAFILES';
DATE:                         'DATE';
DAY:                          'DAY';
DBA_RECYCLEBIN:               'DBA_RECYCLEBIN';
DB_ROLE_CHANGE:               'DB_ROLE_CHANGE';
DBTIMEZONE:                   'DBTIMEZONE';
DEALLOCATE:                   'DEALLOCATE';
DEBUG:                        'DEBUG';
DEC:                          'DEC';
DECIMAL:                      'DECIMAL';
DECLARE:                      'DECLARE';
DECOMPOSE:                    'DECOMPOSE';
DECREMENT:                    'DECREMENT';
DECRYPT:                      'DECRYPT';
DEDUPLICATE:                  'DEDUPLICATE';
DEFAULT:                      'DEFAULT';
DEFAULTS:                     'DEFAULTS';
DEFERRABLE:                   'DEFERRABLE';
DEFERRED:                     'DEFERRED';
DEFINER:                      'DEFINER';
DELEGATE:                     'DELEGATE';
DELETE:                       'DELETE';
DEMAND:                       'DEMAND';
DEPTH:                        'DEPTH';
DESC:                         'DESC';
DETERMINISTIC:                'DETERMINISTIC';
DICTIONARY:                   'DICTIONARY';
DIMENSION:                    'DIMENSION';
DIRECTORY:                    'DIRECTORY';
DIRECT_PATH:                  'DIRECT_PATH';
DISABLE:                      'DISABLE';
DISALLOW:                     'DISALLOW';
DISASSOCIATE:                 'DISASSOCIATE';
DISCONNECT:                   'DISCONNECT';
DISTINCT:                     'DISTINCT';
DISTINGUISHED:                'DISTINGUISHED';
DML:                          'DML';
DOCUMENT:                     'DOCUMENT';
DOUBLE:                       'DOUBLE';
DOWNGRADE:                    'DOWNGRADE';
DROP:                         'DROP';
DSINTERVAL_UNCONSTRAINED:     'DSINTERVAL_UNCONSTRAINED';
EACH:                         'EACH';
EDITIONABLE:                  'EDITIONABLE';
EDITION:                      'EDITION';
EDITIONING:                   'EDITIONING';
EDITIONS:                     'EDITIONS';
ELEMENT:                      'ELEMENT';
ELSE:                         'ELSE';
ELSIF:                        'ELSIF';
EMPTY:                        'EMPTY';
ENABLE:                       'ENABLE';
ENCODING:                     'ENCODING';
ENCRYPT:                      'ENCRYPT';
ENCRYPTION:                   'ENCRYPTION';
END:                          'END';
ENFORCED:                     'ENFORCED';
ENTERPRISE:                   'ENTERPRISE';
ENTITYESCAPING:               'ENTITYESCAPING';
ERR:                          'ERR';
ERRORS:                       'ERRORS';
ESCAPE:                       'ESCAPE';
EVALNAME:                     'EVALNAME';
EXCEPT:                       'EXCEPT';
EXCEPTION:                    'EXCEPTION';
EXCEPTION_INIT:               'EXCEPTION_INIT';
EXCEPTIONS:                   'EXCEPTIONS';
EXCLUDE:                      'EXCLUDE';
EXCLUDING:                    'EXCLUDING';
EXCLUSIVE:                    'EXCLUSIVE';
EXECUTE:                      'EXECUTE';
EXEMPT:                       'EXEMPT';
EXISTS:                       'EXISTS';
EXIT:                         'EXIT';
EXPIRE:                       'EXPIRE';
EXPLAIN:                      'EXPLAIN';
EXTENT:                       'EXTENT';
EXTERNAL:                     'EXTERNAL';
EXTERNALLY:                   'EXTERNALLY';
EXTRACT:                      'EXTRACT';
FAILED:                       'FAILED';
FAILURE:                      'FAILURE';
FALSE:                        'FALSE';
FAST:                         'FAST';
FETCH:                        'FETCH';
FILE:                         'FILE';
FILESYSTEM_LIKE_LOGGING:      'FILESYSTEM_LIKE_LOGGING';
FINAL:                        'FINAL';
FINISH:                       'FINISH';
FIRST:                        'FIRST';
FIRST_VALUE:                  'FIRST_VALUE';
FLASHBACK:                    'FLASHBACK';
FLASH_CACHE:                  'FLASH_CACHE';
FLOAT:                        'FLOAT';
FOLDER:                       'FOLDER';
FOLLOWING:                    'FOLLOWING';
FOLLOWS:                      'FOLLOWS';
FORALL:                       'FORALL';
FORCE:                        'FORCE';
FOREIGN:                      'FOREIGN';
FOR:                          'FOR';
FREELIST:                     'FREELIST';
FREELISTS:                    'FREELISTS';
FREEPOOLS:                    'FREEPOOLS';
FRESH:                        'FRESH';
FROM:                         'FROM';
FULL:                         'FULL';
FUNCTION:                     'FUNCTION';
FUNCTIONS:                    'FUNCTIONS';
GENERATED:                    'GENERATED';
GLOBAL:                       'GLOBAL';
GLOBALLY:                     'GLOBALLY';
GLOBAL_NAME:                  'GLOBAL_NAME';
GOTO:                         'GOTO';
GRANT:                        'GRANT';
GROUP:                        'GROUP';
GROUPING:                     'GROUPING';
GROUPS:                       'GROUPS';
GUARANTEE:                    'GUARANTEE';
GUARD:                        'GUARD';
HASH:                         'HASH';
HASHKEYS:                     'HASHKEYS';
HAVING:                       'HAVING';
HIDE:                         'HIDE';
HIERARCHY:                    'HIERARCHY';
HIGH:                         'HIGH';
HOUR:                         'HOUR';
IDENTIFIED:                   'IDENTIFIED';
IDENTIFIER:                   'IDENTIFIER';
IDENTITY:                     'IDENTITY';
ID:                           'ID';
IF:                           'IF';
IGNORE:                       'IGNORE';
IMMEDIATE:                    'IMMEDIATE';
INCLUDE:                      'INCLUDE';
INCLUDING:                    'INCLUDING';
INCREMENT:                    'INCREMENT';
INDENT:                       'INDENT';
INDEXED:                      'INDEXED';
INDEXES:                      'INDEXES';
INDEX:                        'INDEX';
INDEXTYPE:                    'INDEXTYPE';
INDEXTYPES:                   'INDEXTYPES';
INDICATOR:                    'INDICATOR';
INDICES:                      'INDICES';
INFINITE:                     'INFINITE';
INHERIT:                      'INHERIT';
IN:                           'IN';
INITIAL:                      'INITIAL';
INITIALIZED:                  'INITIALIZED';
INITIALLY:                    'INITIALLY';
INITRANS:                     'INITRANS';
INLINE:                       'INLINE';
INNER:                        'INNER';
INOUT:                        'INOUT';
INSERT:                       'INSERT';
INSTANCE:                     'INSTANCE';
INSTANTIABLE:                 'INSTANTIABLE';
INSTEAD:                      'INSTEAD';
INTEGER:                      'INTEGER';
INTERSECT:                    'INTERSECT';
INTERVAL:                     'INTERVAL';
INT:                          'INT';
INTO:                         'INTO';
INVALIDATE:                   'INVALIDATE';
INVISIBLE:                    'INVISIBLE';
IS:                           'IS';
ISOLATION:                    'ISOLATION';
ITERATE:                      'ITERATE';
JAVA:                         'JAVA';
JOB:                          'JOB';
JOIN:                         'JOIN';
KEEP_DUPLICATES:              'KEEP_DUPLICATES';
KEEP:                         'KEEP';
KEY:                          'KEY';
LANGUAGE:                     'LANGUAGE';
LAST:                         'LAST';
LAST_VALUE:                   'LAST_VALUE';
LEADING:                      'LEADING';
LEFT:                         'LEFT';
LESS:                         'LESS';
LEVEL:                        'LEVEL';
LEVELS:                       'LEVELS';
LIBRARY:                      'LIBRARY';
LIKE2:                        'LIKE2';
LIKE4:                        'LIKE4';
LIKEC:                        'LIKEC';
LIKE:                         'LIKE';
LIMIT:                        'LIMIT';
LINK:                         'LINK';
LIST:                         'LIST';
LOB:                          'LOB';
LOBS:                         'LOBS';
LOCAL:                        'LOCAL';
LOCATOR:                      'LOCATOR';
LOCKED:                       'LOCKED';
LOCK:                         'LOCK';
LOGFILE:                      'LOGFILE';
LOGFILES:                     'LOGFILES';
LOGGING:                      'LOGGING';
LOGICAL:                      'LOGICAL';
LOG:                          'LOG';
LOGMINING:                    'LOGMINING';
LOGOFF:                       'LOGOFF';
LOGON:                        'LOGON';
LONG:                         'LONG';
LOOP:                         'LOOP';
LOW:                          'LOW';
MAIN:                         'MAIN';
MANAGED:                      'MANAGED';
MANAGE:                       'MANAGE';
MANAGEMENT:                   'MANAGEMENT';
MANUAL:                       'MANUAL';
MAP:                          'MAP';
MAPPING:                      'MAPPING';
MASTER:                       'MASTER';
MATCHED:                      'MATCHED';
MATERIALIZED:                 'MATERIALIZED';
MAXEXTENTS:                   'MAXEXTENTS';
MAXIMIZE:                     'MAXIMIZE';
MAXSIZE:                      'MAXSIZE';
MAXVALUE:                     'MAXVALUE';
MEASURE:                      'MEASURE';
MEASURES:                     'MEASURES';
MEDIUM:                       'MEDIUM';
MEMBER:                       'MEMBER';
MERGE:                        'MERGE';
MINEXTENTS:                   'MINEXTENTS';
MINIMIZE:                     'MINIMIZE';
MINIMUM:                      'MINIMUM';
MINING:                       'MINING';
MINUS:                        'MINUS';
MINUTE:                       'MINUTE';
MINVALUE:                     'MINVALUE';
MLSLABEL:                     'MLSLABEL';
MODEL:                        'MODEL';
MODE:                         'MODE';
MODIFY:                       'MODIFY';
MONITORING:                   'MONITORING';
MONTH:                        'MONTH';
MOUNT:                        'MOUNT';
MOVEMENT:                     'MOVEMENT';
MOVE:                         'MOVE';
MULTISET:                     'MULTISET';
NAME:                         'NAME';
NAMESPACE:                    'NAMESPACE';
NAN:                          'NAN';
NATURAL:                      'NATURAL';
NATURALN:                     'NATURALN';
NAV:                          'NAV';
NCHAR_CS:                     'NCHAR_CS';
NCHAR:                        'NCHAR';
NCLOB:                        'NCLOB';
NESTED:                       'NESTED';
NETWORK:                      'NETWORK';
NEVER:                        'NEVER';
NEW:                          'NEW';
NEXT:                         'NEXT';
NOARCHIVELOG:                 'NOARCHIVELOG';
NOAUDIT:                      'NOAUDIT';
NOCACHE:                      'NOCACHE';
NOCOMPRESS:                   'NOCOMPRESS';
NOCOPY:                       'NOCOPY';
NOCYCLE:                      'NOCYCLE';
NODELAY:                      'NODELAY';
NOENTITYESCAPING:             'NOENTITYESCAPING';
NOGUARANTEE:                  'NOGUARANTEE';
NOLOGGING:                    'NOLOGGING';
NOMAPPING:                    'NOMAPPING';
NOMAXVALUE:                   'NOMAXVALUE';
NOMINIMIZE:                   'NOMINIMIZE';
NOMINVALUE:                   'NOMINVALUE';
NOMONITORING:                 'NOMONITORING';
NONEDITIONABLE:               'NONEDITIONABLE';
NONE:                         'NONE';
NO:                           'NO';
NONSCHEMA:                    'NONSCHEMA';
NOORDER:                      'NOORDER';
NOPARALLEL:                   'NOPARALLEL';
NORELY:                       'NORELY';
NORESETLOGS:                  'NORESETLOGS';
NOREVERSE:                    'NOREVERSE';
NORMAL:                       'NORMAL';
NOROWDEPENDENCIES:            'NOROWDEPENDENCIES';
NOSCHEMACHECK:                'NOSCHEMACHECK';
NOSORT:                       'NOSORT';
NOTHING:                      'NOTHING';
NOTIFICATION:                 'NOTIFICATION';
NOT:                          'NOT';
NOVALIDATE:                   'NOVALIDATE';
NOWAIT:                       'NOWAIT';
NULL_:                        'NULL';
NULLS:                        'NULLS';
NUMBER:                       'NUMBER';
NUMERIC:                      'NUMERIC';
OBJECT:                       'OBJECT';
OFFLINE:                      'OFFLINE';
OFF:                          'OFF';
OFFSET:                       'OFFSET';
OF:                           'OF';
OIDINDEX:                     'OIDINDEX';
OID:                          'OID';
OLD:                          'OLD';
OLTP:                         'OLTP';
ONLINE:                       'ONLINE';
ONLY:                         'ONLY';
ON:                           'ON';
OPEN:                         'OPEN';
OPERATOR:                     'OPERATOR';
OPTIMAL:                      'OPTIMAL';
OPTION:                       'OPTION';
ORADATA:                      'ORADATA';
ORDER:                        'ORDER';
ORDINALITY:                   'ORDINALITY';
OR:                           'OR';
OSERROR:                      'OSERROR';
OUTER:                        'OUTER';
OUTLINE:                      'OUTLINE';
OUT:                          'OUT';
OVERFLOW:                     'OVERFLOW';
OVER:                         'OVER';
OVERRIDING:                   'OVERRIDING';
PACKAGE:                      'PACKAGE';
PACKAGES:                     'PACKAGES';
PARALLEL_ENABLE:              'PARALLEL_ENABLE';
PARALLEL:                     'PARALLEL';
PARAMETERS:                   'PARAMETERS';
PARENT:                       'PARENT';
PARTITION:                    'PARTITION';
PARTITIONS:                   'PARTITIONS';
PASSING:                      'PASSING';
PASSWORD:                     'PASSWORD';
PATH:                         'PATH';
PCTFREE:                      'PCTFREE';
PCTINCREASE:                  'PCTINCREASE';
PCTTHRESHOLD:                 'PCTTHRESHOLD';
PCTUSED:                      'PCTUSED';
PCTVERSION:                   'PCTVERSION';
PERCENT_FOUND:                '%' SPACE* 'FOUND';
PERCENT_ISOPEN:               '%' SPACE* 'ISOPEN';
PERCENT_NOTFOUND:             '%' SPACE* 'NOTFOUND';
PERCENT_KEYWORD:              'PERCENT';
PERCENT_ROWCOUNT:             '%' SPACE* 'ROWCOUNT';
PERCENT_ROWTYPE:              '%' SPACE* 'ROWTYPE';
PERCENT_TYPE:                 '%' SPACE* 'TYPE';
PERFORMANCE:                  'PERFORMANCE';
PERMANENT:                    'PERMANENT';
PHYSICAL:                     'PHYSICAL';
PIPELINED:                    'PIPELINED';
PIPE:                         'PIPE';
PIVOT:                        'PIVOT';
PLAN:                         'PLAN';
PLS_INTEGER:                  'PLS_INTEGER';
PLUGGABLE:                    'PLUGGABLE';
POLICY:                       'POLICY';
POSITIVEN:                    'POSITIVEN';
POSITIVE:                     'POSITIVE';
PRAGMA:                       'PRAGMA';
PREBUILT:                     'PREBUILT';
PRECEDING:                    'PRECEDING';
PRECISION:                    'PRECISION';
PREPARE:                      'PREPARE';
PRESENT:                      'PRESENT';
PRESERVE:                     'PRESERVE';
PRIMARY:                      'PRIMARY';
PRIOR:                        'PRIOR';
PRIVILEGE:                    'PRIVILEGE';
PRIVILEGES:                   'PRIVILEGES';
PROCEDURAL:                   'PROCEDURAL';
PROCEDURE:                    'PROCEDURE';
PROCESS:                      'PROCESS';
PROFILE:                      'PROFILE';
PROGRAM:                      'PROGRAM';
PROTECTION:                   'PROTECTION';
PUBLIC:                       'PUBLIC';
PURGE:                        'PURGE';
QUERY:                        'QUERY';
QUOTA:                        'QUOTA';
RAISE:                        'RAISE';
RANGE:                        'RANGE';
RAW:                          'RAW';
READ:                         'READ';
READS:                        'READS';
REAL:                         'REAL';
REBUILD:                      'REBUILD';
RECORD:                       'RECORD';
RECORDS_PER_BLOCK:            'RECORDS_PER_BLOCK';
RECOVER:                      'RECOVER';
RECYCLE:                      'RECYCLE';
REDACTION:                    'REDACTION';
REDUCED:                      'REDUCED';
REFERENCE:                    'REFERENCE';
REFERENCES:                   'REFERENCES';
REFERENCING:                  'REFERENCING';
REF:                          'REF';
REFRESH:                      'REFRESH';
REGISTER:                     'REGISTER';
REJECT:                       'REJECT';
REKEY:                        'REKEY';
RELATIONAL:                   'RELATIONAL';
RELIES_ON:                    'RELIES_ON';
RELY:                         'RELY';
REMOVE:                       'REMOVE';
RENAME:                       'RENAME';
REPLACE:                      'REPLACE';
REPLICATION:                  'REPLICATION';
REQUIRED:                     'REQUIRED';
RESETLOGS:                    'RESETLOGS';
RESIZE:                       'RESIZE';
RESOURCE:                     'RESOURCE';
RESPECT:                      'RESPECT';
RESTRICTED:                   'RESTRICTED';
RESTRICT_REFERENCES:          'RESTRICT_REFERENCES';
RESULT_CACHE:                 'RESULT_CACHE';
RESULT:                       'RESULT';
RESUMABLE:                    'RESUMABLE';
RETENTION:                    'RETENTION';
RETURNING:                    'RETURNING';
RETURN:                       'RETURN';
REUSE:                        'REUSE';
REVERSE:                      'REVERSE';
REVOKE:                       'REVOKE';
REWRITE:                      'REWRITE';
RIGHT:                        'RIGHT';
ROLE:                         'ROLE';
ROLES:                        'ROLES';
ROLLBACK:                     'ROLLBACK';
ROLLUP:                       'ROLLUP';
ROWDEPENDENCIES:              'ROWDEPENDENCIES';
ROWID:                        'ROWID';
ROW:                          'ROW';
ROWS:                         'ROWS';
RULES:                        'RULES';
SALT:                         'SALT';
SAMPLE:                       'SAMPLE';
SAVEPOINT:                    'SAVEPOINT';
SAVE:                         'SAVE';
SCHEDULER:                    'SCHEDULER';
SCHEMACHECK:                  'SCHEMACHECK';
SCHEMA:                       'SCHEMA';
SCN:                          'SCN';
SCOPE:                        'SCOPE';
SEARCH:                       'SEARCH';
SECOND:                       'SECOND';
SECUREFILE:                   'SECUREFILE';
SEED:                         'SEED';
SEGMENT:                      'SEGMENT';
SELECTIVITY:                  'SELECTIVITY';
SELECT:                       'SELECT';
SELF:                         'SELF';
SEQUENCE:                     'SEQUENCE';
SEQUENTIAL:                   'SEQUENTIAL';
SERIALIZABLE:                 'SERIALIZABLE';
SERIALLY_REUSABLE:            'SERIALLY_REUSABLE';
SERVERERROR:                  'SERVERERROR';
SESSION:                      'SESSION';
SESSIONTIMEZONE:              'SESSIONTIMEZONE';
SET:                          'SET';
SETS:                         'SETS';
SETTINGS:                     'SETTINGS';
SHARE:                        'SHARE';
SHOW:                         'SHOW';
SHRINK:                       'SHRINK';
SHUTDOWN:                     'SHUTDOWN';
SIBLINGS:                     'SIBLINGS';
SIGNTYPE:                     'SIGNTYPE';
SIMPLE_INTEGER:               'SIMPLE_INTEGER';
SINGLE:                       'SINGLE';
SIZE:                         'SIZE';
SKIP_:                        'SKIP';
SMALLFILE:                    'SMALLFILE';
SMALLINT:                     'SMALLINT';
SNAPSHOT:                     'SNAPSHOT';
SOME:                         'SOME';
SORT:                         'SORT';
SOURCE:                       'SOURCE';
SPACE_KEYWORD:                'SPACE';
SPECIFICATION:                'SPECIFICATION';
SPLIT:                        'SPLIT';
SQL:                          'SQL';
SQLERROR:                     'SQLERROR';
SQLDATA:                      'SQLDATA';
STANDALONE:                   'STANDALONE';
STANDBY:                      'STANDBY';
START:                        'START';
STARTUP:                      'STARTUP';
STATEMENT_ID:                 'STATEMENT_ID';
STATEMENTS:                   'STATEMENTS';
STATEMENT:                    'STATEMENT';
STATIC:                       'STATIC';
STATISTICS:                   'STATISTICS';
STOP:                         'STOP';
STORAGE:                      'STORAGE';
STORE:                        'STORE';
STRING:                       'STRING';
STRUCTURE:                    'STRUCTURE';
SUBMULTISET:                  'SUBMULTISET';
SUBPARTITIONS:                'SUBPARTITIONS';
SUBPARTITION:                 'SUBPARTITION';
SUBSTITUTABLE:                'SUBSTITUTABLE';
SUBTYPE:                      'SUBTYPE';
SUCCESSFUL:                   'SUCCESSFUL';
SUCCESS:                      'SUCCESS';
SUPPLEMENTAL:                 'SUPPLEMENTAL';
SUSPEND:                      'SUSPEND';
SWITCHOVER:                   'SWITCHOVER';
SWITCH:                       'SWITCH';
SYNCHRONOUS:                  'SYNCHRONOUS';
SYNONYM:                      'SYNONYM';
SYSBACKUP:                    'SYSBACKUP';
SYSDBA:                       'SYSDBA';
SYSDG:                        'SYSDG';
SYSGUID:                      'SYSGUID';
SYSKM:                        'SYSKM';
SYSOPER:                      'SYSOPER';
SYSTEM:                       'SYSTEM';
TABLESPACE:                   'TABLESPACE';
TABLES:                       'TABLES';
TABLE:                        'TABLE';
TEMPFILE:                     'TEMPFILE';
TEMPLATE:                     'TEMPLATE';
TEMPORARY:                    'TEMPORARY';
TEST:                         'TEST';
THAN:                         'THAN';
THEN:                         'THEN';
THE:                          'THE';
THREAD:                       'THREAD';
THROUGH:                      'THROUGH';
TIES:                         'TIES';
TIMESTAMP_LTZ_UNCONSTRAINED:  'TIMESTAMP_LTZ_UNCONSTRAINED';
TIMESTAMP:                    'TIMESTAMP';
TIMESTAMP_TZ_UNCONSTRAINED:   'TIMESTAMP_TZ_UNCONSTRAINED';
TIMESTAMP_UNCONSTRAINED:      'TIMESTAMP_UNCONSTRAINED';
TIME:                         'TIME';
TIMEZONE:                     'TIMEZONE';
TIMEZONE_ABBR:                'TIMEZONE_ABBR';
TIMEZONE_HOUR:                'TIMEZONE_HOUR';
TIMEZONE_MINUTE:              'TIMEZONE_MINUTE';
TIMEZONE_REGION:              'TIMEZONE_REGION';
TO:                           'TO';
TRACE:                        'TRACE';
TRACKING:                     'TRACKING';
TRAILING:                     'TRAILING';
TRANSACTION:                  'TRANSACTION';
TRANSLATE:                    'TRANSLATE';
TRANSLATION:                  'TRANSLATION';
TREAT:                        'TREAT';
TRIGGERS:                     'TRIGGERS';
TRIGGER:                      'TRIGGER';
TRUE:                         'TRUE';
TRUNCATE:                     'TRUNCATE';
TRUSTED:                      'TRUSTED';
TUNING:                       'TUNING';
TYPES:                        'TYPES';
TYPE:                         'TYPE';
UNARCHIVED:                   'UNARCHIVED';
UNBOUNDED:                    'UNBOUNDED';
UNDER:                        'UNDER';
UNDO:                         'UNDO';
UNIFORM:                      'UNIFORM';
UNION:                        'UNION';
UNIQUE:                       'UNIQUE';
UNLIMITED:                    'UNLIMITED';
UNLOCK:                       'UNLOCK';
UNPIVOT:                      'UNPIVOT';
UNRECOVERABLE:                'UNRECOVERABLE';
UNTIL:                        'UNTIL';
UNUSABLE:                     'UNUSABLE';
UNUSED:                       'UNUSED';
UPDATED:                      'UPDATED';
UPDATE:                       'UPDATE';
UPGRADE:                      'UPGRADE';
UPSERT:                       'UPSERT';
UROWID:                       'UROWID';
USAGE:                        'USAGE';
USERS:                        'USERS';
USER:                         'USER';
USE:                          'USE';
USING:                        'USING';
VALIDATE:                     'VALIDATE';
VALUES:                       'VALUES';
VALUE:                        'VALUE';
VARCHAR:                      'VARCHAR';
VARIABLE:                     'VARIABLE';
VARRAYS:                      'VARRAYS';
VARRAY:                       'VARRAY';
VARYING:                      'VARYING';
VERSIONS:                     'VERSIONS';
VERSION:                      'VERSION';
VIEW:                         'VIEW';
VIRTUAL:                      'VIRTUAL';
VISIBLE:                      'VISIBLE';
WAIT:                         'WAIT';
WARNING:                      'WARNING';
WELLFORMED:                   'WELLFORMED';
WHENEVER:                     'WHENEVER';
WHEN:                         'WHEN';
WHERE:                        'WHERE';
WHILE:                        'WHILE';
WITHIN:                       'WITHIN';
WITHOUT:                      'WITHOUT';
WITH:                         'WITH';
WORK:                         'WORK';
WRITE:                        'WRITE';
XDB:                          'XDB';
XMLAGG:                       'XMLAGG';
XMLATTRIBUTES:                'XMLATTRIBUTES';
XMLCAST:                      'XMLCAST';
XMLCOLATTVAL:                 'XMLCOLATTVAL';
XMLELEMENT:                   'XMLELEMENT';
XMLEXISTS:                    'XMLEXISTS';
XMLFOREST:                    'XMLFOREST';
XMLINDEX:                     'XMLINDEX';
XMLNAMESPACES:                'XMLNAMESPACES';
XMLPARSE:                     'XMLPARSE';
XMLPI:                        'XMLPI';
XMLQUERY:                     'XMLQUERY';
XMLROOT:                      'XMLROOT';
XMLSCHEMA:                    'XMLSCHEMA';
XMLSERIALIZE:                 'XMLSERIALIZE';
XMLTABLE:                     'XMLTABLE';
XMLTYPE:                      'XMLTYPE';
XML:                          'XML';
YEAR:                         'YEAR';
YES:                          'YES';
YMINTERVAL_UNCONSTRAINED:     'YMINTERVAL_UNCONSTRAINED';
ZONE:                         'ZONE';
PREDICTION:                   'PREDICTION';
PREDICTION_BOUNDS:            'PREDICTION_BOUNDS';
PREDICTION_COST:              'PREDICTION_COST';
PREDICTION_DETAILS:           'PREDICTION_DETAILS';
PREDICTION_PROBABILITY:       'PREDICTION_PROBABILITY';
PREDICTION_SET:               'PREDICTION_SET';
CUME_DIST:                    'CUME_DIST';
DENSE_RANK:                   'DENSE_RANK';
LISTAGG:                      'LISTAGG';
PERCENT_RANK:                 'PERCENT_RANK';
PERCENTILE_CONT:              'PERCENTILE_CONT';
PERCENTILE_DISC:              'PERCENTILE_DISC';
RANK:                         'RANK';
AVG:                          'AVG';
CORR:                         'CORR';
COVAR_:                       'COVAR_';
DECODE:                       'DECODE';
DDL:                          'DDL';
LAG:                          'LAG';
LEAD:                         'LEAD';
MAX:                          'MAX';
MEDIAN:                       'MEDIAN';
MIN:                          'MIN';
NTILE:                        'NTILE';
NVL:                          'NVL';
NVARCHAR2:                    'NVARCHAR2';
RATIO_TO_REPORT:              'RATIO_TO_REPORT';
REGR_:                        'REGR_';
ROUND:                        'ROUND';
ROW_NUMBER:                   'ROW_NUMBER';
SUBSTR:                       'SUBSTR';
TO_CHAR:                      'TO_CHAR';
TRIM:                         'TRIM';
SUM:                          'SUM';
STDDEV:                       'STDDEV';
VAR_:                         'VAR_';
VARCHAR2:                     'VARCHAR2';
VARIANCE:                     'VARIANCE';
LEAST:                        'LEAST';
GREATEST:                     'GREATEST';
TO_DATE:                      'TO_DATE';

// Rule #358 <NATIONAL_CHAR_STRING_LIT> - subtoken typecast in <REGULAR_ID>, it also incorporates <character_representation>
//  Lowercase 'n' is a usual addition to the standard

NATIONAL_CHAR_STRING_LIT: 'N' '\'' (~('\'' | '\r' | '\n' ) | '\'' '\'' | NEWLINE)* '\'';

//  Rule #040 <BIT_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'b' is a usual addition to the standard

BIT_STRING_LIT: 'B' ('\'' [01]* '\'')+;

//  Rule #284 <HEX_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'x' is a usual addition to the standard

HEX_STRING_LIT: 'X' ('\'' [A-F0-9]* '\'')+;
DOUBLE_PERIOD:  '..';
PERIOD:         '.';

//{ Rule #238 <EXACT_NUM_LIT>
//  This rule is a bit tricky - it resolves the ambiguity with <PERIOD>
//  It also incorporates <mantisa> and <exponent> for the <APPROXIMATE_NUM_LIT>
//  Rule #501 <signed_integer> was incorporated directly in the token <APPROXIMATE_NUM_LIT>
//  See also the rule #617 <unsigned_num_lit>
/*
    : (
            UNSIGNED_INTEGER
            ( '.' UNSIGNED_INTEGER
            | {$type = UNSIGNED_INTEGER;}
            ) ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    | '.' UNSIGNED_INTEGER ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    )
    (D | F)?
    ;*/

UNSIGNED_INTEGER:    [0-9]+;
APPROXIMATE_NUM_LIT: FLOAT_FRAGMENT ('E' ('+'|'-')? (FLOAT_FRAGMENT | [0-9]+))? ('D' | 'F')?;

// Rule #--- <CHAR_STRING> is a base for Rule #065 <char_string_lit> , it incorporates <character_representation>
// and a superfluous subtoken typecasting of the "QUOTE"
CHAR_STRING: '\''  (~('\'' | '\r' | '\n') | '\'' '\'' | NEWLINE)* '\'';

// See https://livesql.oracle.com/apex/livesql/file/content_CIREYU9EA54EOKQ7LAMZKRF6P.html
// TODO: context sensitive string quotes (any characted after quote)
CHAR_STRING_PERL    : 'Q' '\'' (QS_ANGLE | QS_BRACE | QS_BRACK | QS_PAREN | QS_EXCLAM | QS_SHARP | QS_QUOTE | QS_DQUOTE) '\'' -> type(CHAR_STRING);
fragment QS_ANGLE   : '<' .*? '>';
fragment QS_BRACE   : '{' .*? '}';
fragment QS_BRACK   : '[' .*? ']';
fragment QS_PAREN   : '(' .*? ')';
fragment QS_EXCLAM  : '!' .*? '!';
fragment QS_SHARP   : '#' .*? '#';
fragment QS_QUOTE   : '\'' .*? '\'';
fragment QS_DQUOTE  : '"' .*? '"';

DELIMITED_ID: '"' (~('"' | '\r' | '\n') | '"' '"')+ '"' ;

PERCENT:                   '%';
AMPERSAND:                 '&';
LEFT_PAREN:                '(';
RIGHT_PAREN:               ')';
DOUBLE_ASTERISK:           '**';
ASTERISK:                  '*';
PLUS_SIGN:                 '+';
MINUS_SIGN:                '-';
COMMA:                     ',';
SOLIDUS:                   '/';
AT_SIGN:                   '@';
ASSIGN_OP:                 ':=';

BINDVAR
    : ':' SIMPLE_LETTER  (SIMPLE_LETTER | [0-9] | '_')*
    | ':' DELIMITED_ID  // not used in SQL but spotted in v$sqltext when using cursor_sharing
    | ':' UNSIGNED_INTEGER
    | QUESTION_MARK // not in SQL, not in Oracle, not in OCI, use this for JDBC
    ;

NOT_EQUAL_OP:              '!='
            |              '<>'
            |              '^='
            |              '~='
            ;
CARRET_OPERATOR_PART:      '^';
TILDE_OPERATOR_PART:       '~';
EXCLAMATION_OPERATOR_PART: '!';
GREATER_THAN_OP:           '>';
LESS_THAN_OP:              '<';
COLON:                     ':';
SEMICOLON:                 ';';

BAR:       '|';
EQUALS_OP: '=';

LEFT_BRACKET:  '[';
RIGHT_BRACKET: ']';

INTRODUCER: '_';

// Comments https://docs.oracle.com/cd/E11882_01/server.112/e41084/sql_elements006.htm

SINGLE_LINE_COMMENT: '--' ~('\r' | '\n')* NEWLINE_EOF                 -> channel(HIDDEN);
MULTI_LINE_COMMENT:  '/*' .*? '*/'                                    -> channel(HIDDEN);
// https://docs.oracle.com/cd/E11882_01/server.112/e16604/ch_twelve034.htm#SQPUG054
REMARK_COMMENT:      'REM' {IsNewlineAtPos(-4)}? 'ARK'? (' ' ~('\r' | '\n')*)? NEWLINE_EOF -> channel(HIDDEN);

// https://docs.oracle.com/cd/E11882_01/server.112/e16604/ch_twelve032.htm#SQPUG052
PROMPT_MESSAGE:      'PRO' {IsNewlineAtPos(-4)}? 'MPT'? (' ' ~('\r' | '\n')*)? NEWLINE_EOF;

// TODO: should starts with newline
START_CMD
    //: 'STA' 'RT'? SPACE ~('\r' | '\n')* NEWLINE_EOF
    // https://docs.oracle.com/cd/B19306_01/server.102/b14357/ch12002.htm
    // https://docs.oracle.com/cd/B19306_01/server.102/b14357/ch12003.htm
    : '@' {IsNewlineAtPos(-2)}? '@'? ~('\r' | '\n')* NEWLINE_EOF
    ;

REGULAR_ID: SIMPLE_LETTER (SIMPLE_LETTER | '$' | '_' | '#' | [0-9])*;

SPACES: [ \t\r\n]+ -> channel(HIDDEN);

// Fragment rules

fragment NEWLINE_EOF    : NEWLINE | EOF;
fragment QUESTION_MARK  : '?';
fragment SIMPLE_LETTER  : [A-Z];
fragment FLOAT_FRAGMENT : UNSIGNED_INTEGER* '.'? UNSIGNED_INTEGER+;
fragment NEWLINE        : '\r'? '\n';
fragment SPACE          : [ \t];
