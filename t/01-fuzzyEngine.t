use Test::Most;
use List::MoreUtils;

BEGIN { use_ok 'AI::FuzzyEngine::Set'                 };
BEGIN { use_ok 'AI::FuzzyEngine::Variable'            };
BEGIN { use_ok 'AI::FuzzyEngine'                      };

subtest 'FuzzyEngine' => sub {
    my $fe = AI::FuzzyEngine->new();
    isa_ok( $fe, 'AI::FuzzyEngine', '$fe' );

    # Disjunction:
    my $a = $fe->or( 0.2, 0.5, 0.8, 0.7 );
    is( $a, 0.8, 'calculation of "or"' );
    # Conjunction:
    my $b = $fe->and( 0.2, 0.5, 0.8, 0.7 );
    is( $b, 0.2, 'calculation of "and"' );
    # Negation:
    my $c = $fe->not( 0.4 );
    is( $c, 0.6, 'calculation of "not"' );

};

subtest 'FuzzyEngine::Set class functions' => sub {
    my $class   = 'AI::FuzzyEngine::Set';
    my $fun_in  = [[1=>2] => [-1=>1]];
    my $fun_out = $class->_copy_fun( $fun_in );
    ok(    ($fun_out      ne $fun_in     )
        && ($fun_out->[0] ne $fun_in->[0])
        && ($fun_out->[1] ne $fun_in->[1]),
        '_copy_fun copies all references',
      );

    my $fun = [ [10] => [0.5] ];
    $class->set_x_limits( $fun, 0 => 1 );
    is_deeply( $fun,
               [ [0, 1] => [0.5, 0.5] ],
               'set_x_limits, single point', 
             );

    $fun = [ [1, 2] => [1, 1] ];
    $class->set_x_limits( $fun, 0 => 3 );
    is_deeply( $fun,
               [ [0, 1, 2, 3] => [1, 1, 1, 1] ],
               'set_x_limits, enlarge', 
             );

    $fun = [ [-1, 4] => [1, 1] ];
    $class->set_x_limits( $fun, 0 => 3 );
    is_deeply( $fun,
               [ [0, 3] => [1, 1] ],
               'set_x_limits, reduce', 
             );

    $fun = [ [-0.4, -0.2, 1.2, 1.4] => [0, 1, 1, 0] ];
    $class->set_x_limits( $fun, -0.2 => 1.2 );
    is_deeply( $fun,
               [ [-0.2, 1.2] => [1, 1] ],
               'set_x_limits, meet inner points', 
             );

    $fun = [ [-1.2, -1.0, 1.2, 1.4] => [0, 1, 1, 0] ];
    $class->set_x_limits( $fun, -0.2 => 0.2 );
    is_deeply( $fun,
               [ [-0.2, 0.2] => [1, 1] ],
               'set_x_limits skip inner points',
             );

    my $funA = [ [1, 2] => [-1, -2] ];
    my $funB = [ [0, 4] => [-2, -3] ];
    $class->synchronize_funs( $funA, $funB );
    is_deeply( $funA->[0], [0, 1, 2, 4], 'synchronize_funs $funA->x' );
    is_deeply( $funB->[0], [0, 1, 2, 4], 'synchronize_funs $funB->x' );
    # y: borders not clipped, so interpol uses border values directly
    is_deeply( $funA->[1], [-1,    -1,   -2, -2], 'synchronize_funs $funA->y' );
    is_deeply( $funB->[1], [-2, -2.25, -2.5, -3], 'synchronize_funs $funB->y' );

    # crossing
    $funA = [ [0, 1] => [0.5,   2] ];
    $funB = [ [0, 1] => [  2, 1.5] ];
    $class->synchronize_funs( $funA, $funB );
    is_deeply( $funA,
               [ [0, 0.75, 1] => [0.5, 1.625, 2] ],
               'synchronize_funs $funA with crossing curves',
             );
    is_deeply( $funB,
               [ [0, 0.75, 1] => [2, 1.625, 1.5] ],
               'synchronize_funs $funB with crossing curves',
             );

    $funA = [ [] => [] ];
    $funB = [ [] => [] ];
    throws_ok { $class->synchronize_funs( $funA, $funB )
              } qr/is empty/, 'Checks for empty functions';

    $funA = [ [1, 2] => [-1, -2] ];
    $funB = [ [0, 4] => [-2, -3] ];
    is_deeply( $class->min_of_funs( $funA, $funB ),
               [ [0, 1, 2, 4] => [-2, -2.25, -2.5, -3] ],
               'min_of_funs',
             );
    is_deeply( $class->max_of_funs( $funA, $funB ),
               [ [0, 1, 2, 4] => [-1,    -1,   -2, -2] ],
               'max_of_funs',
             );

    my $funC = [ [0, 4] => [-2.75, -2.75] ];
    is_deeply( $class->min_of_funs( $funA, $funB, $funC ),
               [ [0, 1, 2, 3, 4] => [-2.75, -2.75, -2.75, -2.75, -3] ],
               'min_of_funs recursively',
             );

    $funA = [ [0, 1, 2] => [0, 1, 0] ];
    my $funA_clipped = $class->clip_fun( $funA => 0.5 );
    is_deeply( $funA_clipped,
               [ [0, 0.5, 1, 1.5, 2] => [0, 0.5, 0.5, 0.5, 0] ],
               'clip_fun',
             );

    $fun = [ [1, 2] => [1, 1] ];
    my $c   = $class->centroid( $fun );
    is( $c, 1.5, 'centroid box' );

    $fun = [ [1, 4] => [0, 1] ];
    $c   = $class->centroid( $fun );
    is( $c, 3, 'centroid triangle positive slope' );

    $fun = [ [1, 4] => [1, 0] ];
    $c   = $class->centroid( $fun );
    is( $c, 2, 'centroid triangle positive slope' );

    $fun = [ [-2, 0, 0, 3] => [0.75, 0.75, 1, 0] ];
    $c   = $class->centroid( $fun );
    is( $c, 0, 'centroid combination, checking area calculation' );
};

subtest 'FuzzyEngine::Set' => sub {
    my $fe = a_fuzzyEngine();
    my %pars = ( fuzzyEngine => $fe,
                 variable    => a_variable( $fe ),
                 name        => 'few',
                 memb_fun    => [[7, 8] => [0, 1]],
               );
    my $s = AI::FuzzyEngine::Set->new(%pars);
    isa_ok( $s, 'AI::FuzzyEngine::Set', 'What the constructor returns' );

    is_deeply( [     $s->name, $s->memb_fun, $s->variable, $s->fuzzyEngine],
               [@pars{qw(name      memb_fun     variable)},            $fe],
               'Attributes given in the constructor',
             );

    is( $s->degree, 0, 'Initial (internal) membership degree is 0' );

    $s->degree( 0.2 );
    is( $s->degree, 0.2, 'degree can be set by assignment' );

    $s->degree( 0.1 );
    is( $s->degree, 0.2, 'Disjunction of last and new degree' );

    $s->degree( 0.3, 0.5 );
    is( $s->degree, 0.3, 'Conjunction of multiple inputs ("and" operation)' );

    $pars{memb_fun} = [ [0.2, 0.3, 0.8, 1.0], # x
                        [0.1, 0.5, 0.5, 0.0], # y
                      ];
    $s = AI::FuzzyEngine::Set->new(%pars);

    # fuzzify some values
    my @vals     = (  0, 0.2, 0.25, 0.3, 0.5, 0.8, 0.90, 1);
    my @expected = (0.1, 0.1, 0.30, 0.5, 0.5, 0.5, 0.25, 0 );
    my @got      = map { $s->fuzzify($_) } @vals;
    is_deeply( \@got, \@expected,
               'fuzzify incl. corner cases and reset of degree',
             );

    my $degree = $s->fuzzify( 0.2 );
    is( $degree, 0.1, 'fuzzify returns degree' );

    $pars{memb_fun} = [ [0, 1, 1, 2] => [1, 2, 3, 4] ];
    throws_ok {$s = AI::FuzzyEngine::Set->new(%pars)
              } qr/no double/i, 'Checks double interpolation coordinates';
};

subtest 'Regular FuzzyEngine::Variable' => sub {
    my $class    = 'AI::FuzzyEngine::Variable';
    my $memb_fun = $class->_curve_to_fun( [8=>1, 7=>0] );
    is_deeply( $memb_fun, [[7, 8] => [0, 1]], '_curve_to_fun'       );
    is_deeply( $class->_curve_to_fun( [] ),
               [[] => []],
               '_curve_to_fun( [] )',
             );

    my $fe = a_fuzzyEngine();
    my $v  = AI::FuzzyEngine::Variable->new( $fe,
                                             0 => 10,
                                            'low'  => [0, 1, 10, 0],
                                            'high' => [0, 0, 10, 1],
                                           );
    isa_ok( $v, 'AI::FuzzyEngine::Variable', '$v' );

    is( $v->fuzzyEngine, $fe, 'fuzzyEngine is stored' );
    ok( ! $v->is_internal, 'Variable is not internal' );

    is_deeply( [$v->from, $v->to, [ sort keys %{ $v->sets } ] ],
               [       0,     10, [ sort qw(low high)       ] ],
               'Variable attributes and set names',
             );

    ok(   $v->is_valid_set('high'     ), 'is_valid_set (true) ' );
    ok( ! $v->is_valid_set('wrong_set'), 'is_valid_set (false)' );

    my $low_set = $v->sets->{low};
    isa_ok( $low_set, 'AI::FuzzyEngine::Set', 'What variable generates' );
    is_deeply( $low_set->memb_fun,
               [ [0, 10] => [1, 0] ],
               'and receives converted membership functions',
             );

    can_ok( $v, 'low' ); # can_ok needs no description!

    my $degree = $v->low;
    is( $degree, 0, 'initial value for degree of low' );

    $degree = $v->low(0.2, 0.1);
    is( $degree, 0.1, 'and / or for degree of low work' );

    my $w  = AI::FuzzyEngine::Variable->new( $fe,
                                             0 => 2,
                                            'low'  => [0, 1],
                                            'med'  => [0, 0],
                                           );

    is( $v->low, 0.1, 'degree for low stays unchanged from other variables' );
    is( $w->low, 0,   'degree for low of the new variable is independend'   );

    # Completing membership functions with regard to x
    $v  = AI::FuzzyEngine::Variable->new( $fe,
                                          0 => 10,
                                          'low'  => [ 3, 1,  6, 0],
                                          'med'  => [ 5, 0.5],
                                          'high' => [ -5, 0, 15, 1],
                                        );

    is_deeply( $v->sets->{low}->memb_fun(),
               [ [0, 3, 6, 10] => [1, 1, 0, 0] ],
               'borders of membership functions are adapted to from=>to ',
             );

    is_deeply( $v->sets->{med}->memb_fun(),
               [ [0, 10] => [0.5, 0.5] ],
               'even if constant',
             );

    is_deeply( $v->sets->{high}->memb_fun(),
               [ [0, 10] => [0.25, 0.75] ],
               '... limits even when crossing edges',
             );

    $v->fuzzify( 0 );
    is_deeply( [$v->low, $v->med, $v->high],
               [      1,     0.5,     0.25],
               'fuzzify fuzzifies all sets',
             );

    $v->fuzzify( 10 );
    is_deeply( [$v->low, $v->med, $v->high],
               [      0,     0.5,     0.75],
               'fuzzify resets and fuzzifies all sets',
             );

    # Defuzzification
    $v = AI::FuzzyEngine::Variable
        ->new( $fe,
               0 => 2,
               low  => [0 => 1, 1 => 1, 1.00001 => 0, 2 => 0],
               high => [0 => 0, 1 => 0, 1.00001 => 1, 2 => 1],
             );

    $v->low(  1 ); # explicit control for next tests
    $v->high( 0 );
    my $val = sprintf "%.2f", $v->defuzzify();
    is( $val*1, 0.5, 'defuzzy low' );

    $v->reset;
    $v->low(  0 );
    $v->high( 0.5 );
    $val = sprintf "%.2f", $v->defuzzify();
    is( $val*1, 1.5, 'defuzzy high' );

    $v->low( 1 );
    $val = $v->defuzzify();
    ok( ($val > 0.5 && $val < 1), 'defuzzy low + 0.5*high' );

    my @range        = 0..99;
    my @list_of_sets = map { ("s_$_" => [$_,1]) } @range;
    my $x = AI::FuzzyEngine::Variable->new( $fe, 0 => 1, @list_of_sets );
    my @indexes      = map {/(\d+)/} $x->set_names;

    no warnings qw(once);
    my @is_same = List::MoreUtils::pairwise {$a==$b} @range, @indexes;
    ok( ( List::MoreUtils::all {$_} @is_same ),
        q{set_names returns the set's names in correct range},
    );
};

subtest 'Internal FuzzyEngine::Variable' => sub {
    my $class    = 'AI::FuzzyEngine::Variable';

    my $fe = a_fuzzyEngine();
    my $v  = AI::FuzzyEngine::Variable->new( $fe,
                                            'low'  => [0, 1, 10, 0],
                                            'high' => [0, 0, 10, 1],
                                           );
    isa_ok( $v, 'AI::FuzzyEngine::Variable', '$v' );

    is( $v->fuzzyEngine, $fe, 'fuzzyEngine is stored' );
    ok( $v->is_internal, 'Variable is internal' );
    is( ref( $v->sets), 'HASH', 'sets is a HashRef' );

    is_deeply( [$v->from, $v->to, [ sort keys %{ $v->sets } ] ],
               [   undef,  undef, [ sort qw(low high)       ] ],
               'Variable attributes and set names',
             );

    ok(   $v->is_valid_set('high'     ), 'is_valid_set (true) ' );
    ok( ! $v->is_valid_set('wrong_set'), 'is_valid_set (false)' );

    my $low_set = $v->set('low');
    isa_ok( $low_set, 'AI::FuzzyEngine::Set', 'What variable->set returns' );
    is_deeply( $low_set->memb_fun, [[]=>[]], 'Membership function is empty' );

    can_ok( $v, 'low' ); # can_ok needs no description!

    my $degree = $v->low;
    is( $degree, 0, 'initial value for degree of low' );

    $degree = $v->low(0.2, 0.1);
    is( $degree, 0.1, 'and / or for degree of low work' );

    # Test:
    $v->reset;
    is( $v->low, 0, 'reset works' );

    # Throw errors!
    throws_ok { $v->fuzzify(0) } qr/internal/, 'Checks illegal fuzzify call';
    throws_ok { $v->defuzzify  } qr/internal/, 'Checks illegal defuzzify call';
};

subtest 'FuzzyEngine as base' => sub {
    my $fe = AI::FuzzyEngine->new();
    isa_ok( $fe, 'AI::FuzzyEngine', '$fe' );

    my $v = $fe->new_variable( 0 => 10,
                               'low'  => [0, 1, 10, 0],
                               'high' => [0, 0, 10, 1],
                             );
    isa_ok( $v, 'AI::FuzzyEngine::Variable', 'What $fe->new_variable returns' );
    is_deeply( [$v->from, $v->to, [ sort keys %{ $v->sets } ] ],
               [       0,     10, [ sort qw(low high)       ] ],
               'Variable attributes and set names generated by new_variable',
             );


    my $w = $fe->new_variable( 0 => 1,
                               'low'  => [0, 1],
                               'high' => [1, 0],
                             );

    is_deeply( [ $fe->variables() ],
               [$v, $w],
               'Engine stores variables (should be weakened)',
             );

    $v->low( 0.1 );
    $w->low( 0.2 );

    my $v_resetted = $v->reset;
    isa_ok( $v_resetted,
            'AI::FuzzyEngine::Variable',
            'What variable->reset returns',
          ) or exit;
    is( $v->low, 0.0, 'Variable can be resetted'       );
    is( $w->low, 0.2, 'Other variables stay unchanged' );

    my $fe_resetted = $fe->reset();
    isa_ok( $fe_resetted,
            'AI::FuzzyEngine',
            'What fuzzyEngine->reset returns',
          );
    is( $w->low, 0.0, 'FuzzyEngine resets all variables' );
};


done_testing();

sub a_variable {
    # Careful!
    # a_variable does not register its result into $fuzzyEngine.
    # ==> is missing in $fe->variables;
    #
    my ($fuzzyEngine, %pars) = shift;
    my $v = AI::FuzzyEngine::Variable->new( $fuzzyEngine,
                                            0 => 1,
                                            'low'  => [0, 0],
                                            'high' => [1, 1],
                                            %pars,
                                          );
    return $v;
}

sub a_fuzzyEngine { return AI::FuzzyEngine->new() }
