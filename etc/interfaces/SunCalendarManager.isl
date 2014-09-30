(* SunCalendarManager.isl *)
(* Copyright (c) 1993 by Xerox Corporation.  All rights reserved. *)
(* Last tweaked by Mike Spreitzer December 13, 1993 4:08 pm PST *)

INTERFACE SunCalendarManager;

TYPE Buffer = SEQUENCE OF SHORT CHARACTER;
TYPE Transaction = ENUMERATION add, cm-remove END;
TYPE WeekIndex = ENUMERATION
	first, second, third, fourth, fifth, last END;
TYPE Interval = ENUMERATION
	single, daily, weekly, biweekly, monthly, yearly,
	nthWeekday, everyNthDay, everyNthWeek,
	everyNthMonth, otherPeriod END;
TYPE Period = RECORD period: Interval, nth: INTEGER END;
TYPE Event-Type = ENUMERATION
	appointment, reminder, otherTag, holiday, toDo END;
TYPE TagList = OPTIONAL Tag;
TYPE Tag = RECORD
	tag: Event-Type, showtime: INTEGER, next: TagList END;
TYPE Privacy-Level = ENUMERATION public, private, semiprivate END;
TYPE Attr = OPTIONAL Attribute;
TYPE Attribute = RECORD
	next: Attr,
	attr: Buffer,
	value: Buffer,
	clientdata: Buffer END;
TYPE Exn = OPTIONAL Except;
TYPE Except = RECORD
	ordinal: INTEGER,
	next: Exn END;
TYPE Id = RECORD
	tick: INTEGER,
	key: INTEGER END;
TYPE UidList = OPTIONAL Uid;
TYPE Uid = RECORD
	appt-id: Id,
	next: UidList END;
TYPE Appt-Status = ENUMERATION
	active, pendingAdd, pendingDelete, committed, cancelled, completed
	END;
TYPE ApptList = OPTIONAL Appt;
TYPE Appt = RECORD
	appt-id: Id,
	tag: TagList,
	duration: INTEGER,
	ntimes: INTEGER,
	what: Buffer,
	period: Period,
	author: Buffer,
	client-data: Buffer,
	exn: Exn,
	attr: Attr,
	appt-status: Appt-Status,
	privacy: Privacy-Level,
	next: ApptList
	END;
TYPE Abb-Appt-List = OPTIONAL Abb-Appt;
TYPE Abb-Appt = RECORD
	appt-id: Id,
	tag: TagList,
	what: Buffer,
	duration: INTEGER,
	period: Period,
	next: Abb-Appt-List,
	appt-status: Appt-Status,
	privacy: Privacy-Level
	END;
TYPE Apptid = RECORD oid: OptId, new-appt: ApptList END;
TYPE OptId = OPTIONAL Id;
TYPE Reminder = RECORD
	appt-id: Id,
	tick: INTEGER,
	attr: Attribute,
	next: ReminderList
	END;
TYPE ReminderList = OPTIONAL Reminder;
TYPE Table-Res-Type = ENUMERATION AP, RM, AB, ID END;
TYPE Table-Res-List = UNION
	ApptList, ReminderList, Abb-Appt-List, UidList END;
TYPE Access-Status = ENUMERATION
	ok, added, removed, failed, exists, partial, other END;
TYPE Table-Res = RECORD status: Access-Status, res: Table-Res-List END;
TYPE Access-Entry = RECORD
	who: Buffer, access-type: INTEGER,
	next: Access-Entry-List END;
TYPE Access-Entry-List = OPTIONAL Access-Entry;
TYPE Access-Args = RECORD
	target: Buffer, access-list: Access-Entry-List END;
TYPE Range = RECORD key1: INTEGER, key2: INTEGER, next: RangeList END;
TYPE RangeList = OPTIONAL Range;
TYPE Keyrange = RECORD
	key: INTEGER,
	tick1: INTEGER, tick2: INTEGER, next: KeyrangeList END;
TYPE KeyrangeList = OPTIONAL Keyrange;
TYPE Tick = INTEGER;
TYPE Args = UNION Tick, Apptid, Uid, Appt, Range, Keyrange END;
TYPE Registration = RECORD
	target: Buffer,
	prognum: CARDINAL,
	versnum: CARDINAL,
	procnum: CARDINAL,
	next: RegistrationList,
	pid: INTEGER END;
TYPE RegistrationList = OPTIONAL Registration;
TYPE Table-Status = ENUMERATION
	ok, duplicate, badtable, notable, denied, other END;
TYPE Registration-Status = ENUMERATION
	registered, failed, deregistered, confused END;

TYPE T = CLASS SINGLETON METHODS
	Ping(),
	Lookup(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Lookup-next-larger(target: Buffer, args: Args, pid: INTEGER)
			  : Table-Res,
	Lookup-next-smaller(target: Buffer, args: Args, pid: INTEGER)
			   : Table-Res,
	Lookup-range(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Abbreviated-Lookup-range(target: Buffer, args: Args,
				 pid: INTEGER): Table-Res,
	Insert(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Delete(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Delete-Instance(target: Buffer, args: Args, pid: INTEGER)
		       : Table-Res,
	Change(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Change-instance(target: Buffer, args: Args, pid: INTEGER)
		       : Table-Res,
	Lookup-Next-Reminder(target: Buffer, args: Args, pid: INTEGER)
			    : Table-Res,
	Check(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Flush(target: Buffer, args: Args, pid: INTEGER): Table-Res,
	Size(target: Buffer, args: Args, pid: INTEGER): INTEGER,
	Register-Callback(reg: Registration): Registration-Status,
	Deregister-Callback(reg: Registration): Registration-Status,
	Set-Access(args: Access-Args): Access-Status,
	Get-Access(args: Access-Args): Access-Args,
	Abbreviated-Lookup-Key-Range(target: Buffer, args: Args,
				     pid: INTEGER): Table-Res,
	GMTOff(): Table-Res
END;
