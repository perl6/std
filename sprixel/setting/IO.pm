
class IO {
    # install constructor
}

sub jseval is export {}

sub say is export { jseval 'say(args.join(""))' }
