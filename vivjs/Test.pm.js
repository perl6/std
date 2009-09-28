function do_ok(expr,msg){
    say((this.result = new p6builtin.Bool(expr.toBool())).v
        ? 'ok'
        : 'nok '+msg);
}

function do_is(left,right,msg){
    say((this.result = new p6builtin.Bool(
        left.WHAT()=='Int()' ? left.do_infix__S_EqualEqual(right).v
      : left.WHAT()=='Bool()' ? left.v === right.toBool().v
      : left.WHAT()=='Str()' ? left.v.do_infix__S_eq(right).v
      : false
    )).v
        ? 'ok'
        : 'nok '+msg);
}

function do_isnt(left,right,msg){
    say((this.result = new p6builtin.Bool(!(
        left.WHAT()=='Int()' ? left.do_infix__S_EqualEqual(right).v
      : left.WHAT()=='Bool()' ? left.v === right.toBool().v
      : left.WHAT()=='Str()' ? left.v.do_infix__S_eq(right).v
      : false
    ))).v
        ? 'ok'
        : 'nok '+msg);
}

function do_plan(planned){
    this.result = new p6builtin.Bool(true);
    say('1..'+planned);
}

function test_stub(left,right,msg){
    this.result = new p6builtin.Bool(false);
    say('nok');
}

// install the JSSUBS in place of the p6subs for which they're standing in
p6toplevel["ok"] = new p6builtin.jssub(do_ok, 'ok');
p6toplevel["is"] = new p6builtin.jssub(do_is, 'is');
p6toplevel["isnt"] = new p6builtin.jssub(do_isnt, 'isnt');
p6toplevel["plan"] = new p6builtin.jssub(do_plan, 'plan');
// install standin fakes for the test routines not implemented
p6toplevel["dies_ok"] = new p6builtin.jssub(test_stub, 'dies_ok');
p6toplevel["lives_ok"] = new p6builtin.jssub(test_stub, 'lives_ok');
p6toplevel["skip"] = new p6builtin.jssub(test_stub, 'skip');
p6toplevel["todo"] = new p6builtin.jssub(test_stub, 'todo');
p6toplevel["pass"] = new p6builtin.jssub(test_stub, 'pass');
p6toplevel["dies_ok"] = new p6builtin.jssub(test_stub, 'dies_ok');
p6toplevel["flunk"] = new p6builtin.jssub(test_stub, 'flunk');
p6toplevel["force_todo"] = new p6builtin.jssub(test_stub, 'force_todo');
p6toplevel["use_ok"] = new p6builtin.jssub(test_stub, 'use_ok');
p6toplevel["isa_ok"] = new p6builtin.jssub(test_stub, 'isa_ok');
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