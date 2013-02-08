package AI::FuzzyEngine::Variable;

use version; our $VERSION = qv('v0.1.0');

use strict;
use warnings;
use Scalar::Util qw( blessed looks_like_number );
use List::MoreUtils;
use Carp;

use AI::FuzzyEngine::Set;

sub new {
    my ($class, $fuzzyEngine, @pars) = @_;
    my $self = bless {}, $class;

    # check and store the assigned fuzzyEngine
    my $fe_class = 'AI::FuzzyEngine';
    croak "fuzzyEngine is not a $fe_class"
        unless blessed $fuzzyEngine && $fuzzyEngine->isa($fe_class);
    $self->{fuzzyEngine} = $fuzzyEngine;

    # load pars, create sets etc.
    $self->_init(@pars);

    return $self;
};

sub is_internal {   shift->{is_internal} }
sub from        {   shift->{from}        };
sub to          {   shift->{to}          };
sub sets        {   shift->{sets}        };
sub set_names   { @{shift->{set_names}}  };
sub set {
    my ($self, $set_name) = @_;
    return $self->{sets}{$set_name};
};
sub fuzzyEngine { shift->{fuzzyEngine} };

sub is_valid_set {
    my ($self, $set_name) = @_;
    return List::MoreUtils::any { $_ eq $set_name } keys %{ $self->sets };
}

sub fuzzify {
    my ($self, $val) = @_;
    croak "Fuzzification not allowed for internal variables"
        if $self->is_internal;
    for my $set (values %{ $self->sets } ) {
        $set->fuzzify( $val );
    };
    return;
}

sub defuzzify {
    my ($self)  = @_;
    croak "Defuzzification not allowed for internal variables"
        if $self->is_internal;

    my $s_class = 'AI::FuzzyEngine::Set';

    my @sets    = values %{$self->sets};
    my @funs    = map {$_->clip_fun( $_->memb_fun => $_->degree ) } @sets;

    my $fun_agg = $s_class->max_of_funs( @funs );
    my $c       = $s_class->centroid( $fun_agg );
    return $c;
}

sub reset {
    my ($self) = @_;
    $_->reset() for values %{$self->sets};
    return $self;
};

sub _init {
    my ($self, @pars) = @_;

    croak "Too few arguments" unless @pars >= 2;

    # Test for internal variable
    my ($from, $to, @sets);
    if (looks_like_number $pars[0]) {
        # $from => $to is given
        $self->{is_internal} = '';
        ($from, $to, @sets)  = @pars;
    }
    else {
        $self->{is_internal} = 1;
        ($from, $to, @sets)  = (undef, undef, @pars);
    };

    # Store $from, $to ( undef if is_internal)
    $self->{from} = $from;
    $self->{to  } = $to;

    # Provide names of sets in correct order by attribute set_names
    my $ix = 1;
    $self->{set_names} = [ grep {$ix++ % 2} @sets ];


    # Build sets of the variable
    my %sets = @sets;
    SET_TO_BUILD:
    for my $set_name (keys %sets) {

        my $fun = [ [] => [] ]; # default membership function

        if (not $self->is_internal) {
            # Convert from set of points to [ \@x, \@y ] format
            my $curve = $sets{$set_name};
            $fun   = $self->_curve_to_fun( $curve );

            # clip membership function to borders
            AI::FuzzyEngine::Set->set_x_limits( $fun, $self->from => $self->to );
        };

        # create a set and store it
        my $set = AI::FuzzyEngine::Set
            ->new( fuzzyEngine => $self->fuzzyEngine,
                   variable    => $self,
                   name        => $set_name,
                   memb_fun    => $fun, # [ [] => [] ] if is_internal
              );
        $self->{sets}{$set_name} = $set;

        # build membership function if necessary
        next SET_TO_BUILD if $self->can( $set_name );
        my $method = sub {
            my ($variable, @vals) = @_; # Variable, fuzzy values
            my $set = $variable->{sets}{$set_name};
            return $set->degree( @vals );
        };

        # register the new method to $self (the fuzzy variable)
        no strict 'refs';
        *{ $set_name } = $method;
    };
}

sub _curve_to_fun {
    # Convert input format for membership functions
    # to internal representation:
    # [$x11, $y11, $x12, $y12, ... ]
    # --> [ $x11, $x12,  ... ] => [$y11, $y12, ... ] ]
    my ($class, $curve) = @_;
    my %points = @$curve;
    my @x      = sort {$a<=>$b} keys %points;
    my @y      = @points{ @x };
    return [ \@x, \@y ];
}



1;

=pod

=head1 NAME

AI::FuzzyEngine::Variable - Class used by AI::FuzzyEngine.

=head1 DESCRIPTION

Please see L<AI::FuzzyEngine> for a description.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc AI::FuzzyEngine

=head1 AUTHOR

Juergen Mueck, jurgen.muck@yahoo.de

=head1 COPYRIGHT

Copyright (c) Juergen Mueck 2013.  All rights reserved.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

