package AI::FuzzyEngine;

use 5.006;
use version; our $VERSION = qv('v0.1.0');

use strict;
use warnings;
use Scalar::Util;
use List::Util;

use AI::FuzzyEngine::Variable;

sub new {
    my ($class) = @_;
    my $self = bless {}, $class;

    $self->{_variables} = [];
    return $self;
}

sub variables { @{ shift->{_variables} } };

sub and {
    my ($self, @vals) = @_;
    return List::Util::min( @vals );
}

sub or {
    my ($self, @vals) = @_;
    return List::Util::max( @vals );
}

sub not {
    my ($self, $val) = @_;
    return 1-$val;
}

sub new_variable {
    my ($self, @pars) = @_;

    my $var = AI::FuzzyEngine::Variable->new($self, @pars);
    push @{$self->{_variables}}, $var;
    Scalar::Util::weaken $self->{_variables}->[-1];
    return $var;
}

sub reset {
    my ($self) = @_;
    $_->reset() for $self->variables(); 
    return $self;
}

1;

=pod

=head1 NAME

AI::FuzzyEngine - A small Fuzzy Engine

=head1 SYNOPSIS

    use AI::FuzzyEngine;

    # Engine (or factory) provides fuzzy logical arithmetic
    my $fe = AI::FuzzyEngine->new();

    # Disjunction:
    my $a = $fe->or ( 0.2, 0.5, 0.8, 0.7 ); # 0.8
    # Conjunction:
    my $b = $fe->and( 0.2, 0.5, 0.8, 0.7 ); # 0.2
    # Negation:
    my $c = $fe->not( 0.4 );                # 0.6

    # These functions are constitutive for the operations
    # on the fuzzy sets of the fuzzy variables:

    # VARIABLES (AI::FuzzyEngine::Variable)

    # input variables need definition of membership functions of their sets
    my $flow = $fe->new_variable( 0 => 2000,
                        small => [0, 1,  500, 1, 1000, 0                  ],
                        med   => [       500, 0, 1000, 1, 1500, 0         ],
                        huge  => [               1000, 0, 1500, 1, 2000, 1],
                   );
    my $cap  = $fe->new_variable( 0 => 1800,
                        avg   => [0, 1, 1500, 1, 1700, 0         ],
                        high  => [      1500, 0, 1700, 1, 1800, 1],
                   );
    # internal variables need sets, but no membership functions
    my $saturation = $fe->new_variable( # from => to may be ommitted
                        low   => [],
                        crit  => [],
                        over  => [],
                   );
    # But output variables need membership functions for their sets:
    my $green = $fe->new_variable( -5 => 5,
                        decrease => [-5, 1, -2, 1, 0, 0            ],
                        ok       => [       -2, 0  0, 1, 2, 0      ],
                        increase => [              0, 0, 2, 1, 5, 1],
                   );

    # Reset FuzzyEngine (resets all variables)
    $fe->reset();

    # Reset a fuzzy variable directly
    $flow->reset;

    # Fuzzification of input variables
    $flow->fuzzify( 600 );
    $cap->fuzzify( 1000 );

    # Membership degrees of the respective sets are now available:
    my $flow_is_small = $flow->small(); # 0.8
    my $flow_is_med   = $flow->med();   # 0.2
    my $flow_is_huge  = $flow->huge();  # 0.0

    # RULES and their application

    # a) first step, result is $saturation, an intermediate set
    # implicit application of 'and'
    # Multiple calls to a membership function
    # are similar to 'or' operations:
    $saturation->low( $flow->small(), $cap->avg()  );
    $saturation->low( $flow->small(), $cap->high() );
    $saturation->low( $flow->med(),   $cap->high() );

    # Explicite 'or', 'and' or 'not' possible:
    $saturation->crit( $fe->or( $fe->and( $flow->med(),  $cap->avg()  ),
                                $fe->and( $flow->huge(), $cap->high() ),
                       ),
                 );

    $saturation->over( $fe->not( $flow->small() ),
                       $fe->not( $flow->med()   ),
                       $flow->huge(),
                       $cap->high(),
                 );
    $saturation->over( $flow->huge(), $fe->not( $cap->high() ) );

    # b) second step, deduce output variable from internal state of saturation
    $green->decrease( $saturation->low()  );
    $green->ok(       $saturation->crit() );
    $green->increase( $saturation->over() );

    # All sets provide the respective membership degrees of their variables: 
    my $saturation_is_over = $saturation->over(); # no defuzzification!
    my $green_is_ok        = $green->ok();

    # Defuzzification ( is a matter of the fuzzy set )
    my $delta_green = $green->defuzzify(); # -5 ... 5

=head1 EXPORT

Nothing is exported or exportable.

=head1 DESCRIPTION

This module is yet another implementation of a fuzzy inference system.
The aim was to  be able to code rules (no string parsing),
but avoid operator overloading,
and make it possible to split calculation into multiple steps.
All intermediate results (memberships of sets of variables)
should be available, and there should be no need to defuzzify variables
just to compute any first variables.

Credits to Ala Qumsieh and his L<AI::FuzzyInference>,
that showed me that fuzzy is no magic.
I learned a lot by analyzing his code,
and he provides good information and links to learn more about Fuzzy Logics.

=head2 Fuzzy stuff

The L<AI::FuzzyEngine> object defines and provides
the elementary operations for fuzzy sets.
All set memberships are values from 0 to 1.
Up to now there is no choice with regard how to operate on sets:

=over 2

=item C<< $fe->or( ... ) >> (Disjunction)

I<Maximum> of membership degrees

=item C<< $fe->and( ... ) >> (Conjunction)

I<Minimum> of membership degrees

=item C<< $fe->not( $var->$set ) >> (Negation)

I<1-degree> of membership

=item Aggregation of rules (Disjunction)

I<Maximum>

=back

Defuzzification is based on

=over 2

=item Implication

I<Clip> membership function of a set according to membership degree, before the implicated memberships of all sets of a variable are taken for defuzzification: 

=item Defuzzification

I<Centroid> of aggregated (and clipped) membership functions

=back

=head2 Public functions

Creation of an C<AI::FuzzyEngine> object by

    my $fe = AI::FuzzyEngine->new();

This function has no parameters, but provides the fuzzy methods
C<or>, C<and> and C<not>, as listed above.
I plan to introduce alternative fuzzy operations,
they will be configured as arguments to C<new>. 

Once created, the engine can create fuzzy variables by C<new_variable>:

    my $var = $fe->new_variable( $from => $to,
                        $name_of_set1 => [$x11, $y11, $x12, $y12, ... ],
                        $name_of_set2 => [$x21, $y21, $x22, $y22, ... ],
                        ...
                   );

Result is an L<AI::FuzzyEngine::Variable>.
The name_of_set strings are taken to assign corresponding methods
for the respective fuzzy variables.
They must be valid function identifiers.

Fuzzy variables provide a method to fuzzify input values:

    $var->fuzzify( $val );

according to the defined sets and their membership functions.

The memberships of the sets of $var are accessible
by the respective functions:

    my $membership_degree = $var->$name_of_set();

Memberships can be assigned directly (within rules for example):

    $var->$name_of_set( $membership_degree );

If multiple membership_degrees are given, they are "anded":

    $var->$name_of_set( $degree1, $degree2, ... ); # "and"

By this, simple rules can be coded directly:

    my $var_3->zzz( $var_1->xxx, $var_2->yyy, ... ); # "and"

this implements the fuzzy implication

    if $var_1->xxx and $var_2->yyy and ... then $var_3->zzz

The membership degrees of a variable's sets can be reset to undef:

    $var->reset(); # resets a variable
    $fe->reset();  # resets all variables

The fuzzy engine C<$fe> has all variables registered
that have been created by its C<new_variable> method.

A variable can be defuzzified:

    my $out_value = $var->defuzzify();

Sometimes internal variables are used that need neither fuzzification
nor defuzzification.
They can be created by a simplified call to C<new_variable>:

    my $var_int = $fe->new_variable( $name_of_set1 => [],
                                     $name_of_set2 => [],
                                     ...
                       );

Hence, they can not use the methods C<fuzzify> or C<defuzzify>.

Fuzzy operations are simple operations on floating values between 0 and 1:

    my $conjunction = $fe->and( $var1->xxx, $var2->yyy, ... );
    my $disjunction = $fe->or(  $var1->xxx, $var2->yyy, ... );
    my $negated     = $fe->not( $var1->zzz );

There is no magic.

A sequence of rules for the same set can be implemented as follows: 

    $var_3->zzz( $var_1->xxx, $var_2->yyy, ... );
    $var_3->zzz( $var_4->aaa, $var_5->bbb, ... );

The subsequent application of C<< $var_3->zzz(...) >>
corresponds to "or" operations (aggregation of rules).

Only a reset can reset C<$var_3>. 

=head2 Todos

=over 2

=item Add optional alternative implementations of fuzzy operations

=item More checks on input arguments and allowed method calls

=item Make the module PDL aware

=item Split tests into API tests and test of internal functions

=back

=head1 CAVEATS / BUGS

This is my first module.
I'm happy about feedback that helps me to learn
and improve my contributions to the Perl ecosystem.

Please report any bugs or feature requests to
C<bug-ai-fuzzyengine at rt.cpan.org>, or through
the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=AI-FuzzyEngine>.
I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc AI::FuzzyEngine


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=AI-FuzzyEngine>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/AI-FuzzyEngine>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/AI-FuzzyEngine>

=item * Search CPAN

L<http://search.cpan.org/dist/AI-FuzzyEngine/>

=back

=head1 AUTHOR

Juergen Mueck, jurgen.muck@yahoo.de

=head1 COPYRIGHT

Copyright (c) Juergen Mueck 2013.  All rights reserved.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
