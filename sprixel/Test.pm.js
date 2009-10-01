var test_number;

function do_ok(expr,msg){
    say(((this.result = new p6builtin.Bool(expr.toBool && expr.toBool())).v
        ? 'ok '
        : 'not ok ') + test_number++ + (msg ? ' - ' + msg : ''));
}

function do_is(left,right,msg){
    say(((this.result = new p6builtin.Bool(
        right.WHAT()=='Str()' ? left.toString()==right.v
      : left.WHAT()=='Int()' ? (left.do_infix__S_EqualEqual(right).v
            || left.toString() == right.toString())
      : (left.WHAT()=='Bool()' && right.toBool) ? left.v === right.toBool().v
      : left.WHAT()=='Str()' ? left.do_infix__S_eq(right).v
      : left.WHAT()=='Num()' ? left.do_infix__S_EqualEqual(right).v : false
    )).v
        ? 'ok '
        : 'not ok ') + test_number++ + (msg ? ' - ' + msg : ''));
}

function do_isa_ok(left,right,msg){
    say(((this.result = new p6builtin.Bool(
        left.WHAT()==((right.WHAT()=='Str()' && !right.isUndefined)
            ? this.context[right].WHAT() : right.WHAT())
    )).v
        ? 'ok '
        : 'not ok ') + test_number++ + (msg ? ' - ' + msg : ''));
}

function do_isnt(left,right,msg){
    say(((this.result = new p6builtin.Bool(!(
        left.WHAT()=='Int()' ? left.do_infix__S_EqualEqual(right).v
      : (left.WHAT()=='Bool()' && right.toBool) ? left.v === right.toBool().v
      : left.WHAT()=='Str()' ? left.do_infix__S_eq(right).v
      : false
    ))).v
        ? 'ok '
        : 'not ok ') + test_number++ + (msg ? ' - ' + msg : ''));
}

function do_plan(planned){
    this.result = new p6builtin.Bool(true);
    say('1..'+planned);
    test_number = 1;
}

function do_pass(msg){
    say('ok ' + test_number++ + (msg ? ' - ' + msg : ''));
}

function do_flunk(msg){
    say('not ok ' + test_number++ + (msg ? ' - ' + msg : ''));
}

function test_stub(left,right,msg){
    this.result = new p6builtin.Bool(false);
    say(('ok ') + test_number++ + ' TODO '+(msg ? ' - ' + msg : ''));
}

// install the JSSUBS in place of the p6subs for which they're standing in
p6toplevel["ok"] = new p6builtin.jssub(do_ok, 'ok');
p6toplevel["is"] = new p6builtin.jssub(do_is, 'is');
p6toplevel["isnt"] = new p6builtin.jssub(do_isnt, 'isnt');
p6toplevel["plan"] = new p6builtin.jssub(do_plan, 'plan');
p6toplevel["pass"] = new p6builtin.jssub(do_pass, 'pass');
p6toplevel["flunk"] = new p6builtin.jssub(do_flunk, 'flunk');
p6toplevel["isa_ok"] = new p6builtin.jssub(do_isa_ok, 'isa_ok');
// install standin fakes for the test routines not implemented
p6toplevel["dies_ok"] = new p6builtin.jssub(test_stub, 'dies_ok');
p6toplevel["lives_ok"] = new p6builtin.jssub(test_stub, 'lives_ok');
p6toplevel["skip"] = new p6builtin.jssub(test_stub, 'skip');
p6toplevel["todo"] = new p6builtin.jssub(test_stub, 'todo');
p6toplevel["dies_ok"] = new p6builtin.jssub(test_stub, 'dies_ok');
p6toplevel["force_todo"] = new p6builtin.jssub(test_stub, 'force_todo');
p6toplevel["use_ok"] = new p6builtin.jssub(test_stub, 'use_ok');
p6toplevel["cmp_ok"] = new p6builtin.jssub(test_stub, 'cmp_ok');
p6toplevel["diag"] = new p6builtin.jssub(test_stub, 'diag');
p6toplevel["is_deeply"] = new p6builtin.jssub(test_stub, 'is_deeply');
p6toplevel["like"] = new p6builtin.jssub(test_stub, 'like');
p6toplevel["skip_rest"] = new p6builtin.jssub(test_stub, 'skip_rest');
p6toplevel["unlike"] = new p6builtin.jssub(test_stub, 'unlike');
p6toplevel["skip_rest"] = new p6builtin.jssub(test_stub, 'skip_rest');
p6toplevel["eval_dies_ok"] = new p6builtin.jssub(test_stub, 'eval_dies_ok');
p6toplevel["eval_lives_ok"] = new p6builtin.jssub(test_stub, 'eval_lives_ok');
p6toplevel["approx"] = new p6builtin.jssub(test_stub, 'approx');
p6toplevel["is_approx"] = new p6builtin.jssub(test_stub, 'is_approx');
p6toplevel["throws_ok"] = new p6builtin.jssub(test_stub, 'throws_ok');
p6toplevel["version_lt"] = new p6builtin.jssub(test_stub, 'version_lt');

1;
