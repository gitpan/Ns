package Net::Telnet::Ns1000;

#-----------------------------------------------------------------
#
# Net::Telnet::Ns1000 - Control netscreen-10000 firewalls 
#
# by Marcus Ramberg <marcus.ramberg@tele1europe.no>
# Lots of code ripped from Net::Telnet::Cisco;
#
#-----------------------------------------------------------------

use strict;
use Net::Telnet 3.02;
use AutoLoader;
use Carp;

use vars qw($AUTOLOAD @ISA $VERSION);

@ISA      = qw (Net::Telnet);
$VERSION  = '0.01';


#------------------------------
# New Methods
#------------------------------


# ping a host, return true if can be reached
sub ping {
    my ($self,$host)=@_;
    if ($self->cmd('ping '.$host)) {
      if ($self->lastline =~ /Success Rate rate is (\d+) percent/) {
        return $1;
      }
    }
    return 0;  
}

# Displays the last prompt.
sub last_prompt {
    my $self = shift;
    my $stream = $ {*$self}{net_telnet_ns1000};
    exists $stream->{last_prompt} ? $stream->{last_prompt} : undef;
}


# Displays the last command.
sub last_cmd {
    my $self = shift;
    my $stream = $ {*$self}{net_telnet_ns1000};
    exists $stream->{last_cmd} ? $stream->{last_cmd} : undef;
}

# Displays the current vsys
sub current_vsys { 
  #$_[0]->last_prompt =~ /\(([\w.-])\)/ ? $1 : undef;
  $_[0]->last_prompt =~ /\(([\w.-]+)\)/ ? $1 : undef;
}

# enter a vsys
sub enter_vsys { 
  my ($self, $vsys) = @_;
  if ($self->current_vsys) {
     $self->exit(1);
  }
  my %vsys = $self->get_vsys();
  if (exists $vsys{$vsys}) {
    if ($self->cmd('enter vsys '.$vsys)) {
      return 1;
    } else {
      return $self->error('Error entering vsys');
    }
  } else { return $self->error("Vsys not found");}
}

# exit a vsys or main {
sub exit { 
  my ($self,$save) =@_;
  if ($self->current_vsys) {
    $self->cmd('save');
    $self->cmd('exit');
  } else {
    $self->cmd('save');
    $self->close;
  }
}
#    ($prematch, $match) = $self->waitfor(-match => '/[sS]ave\?/',
#					 -match => $cmd_prompt}
#	or do {
#	    return &$error("read eof waiting for save prompt, no changes?")
#		if $self->eof;
#	    return &$error("timed-out waiting for save prompt.");
#	};
#}


#get a hash of the vsys in existence.
sub get_vsys {
  my $self = shift;
  my (%vsys,$result,$backupsys);
  if ($self->current_vsys ne '') {
    $backupsys=$self->current_vsys;
    $self->cmd('exit');
  }
  my @results = $self->getValue("vsys");
  if ($backupsys) {$self->enter_vsys($backupsys);}
  foreach $result (@results) {
    if ($result=~/([\w.-]+)\s+(\d+)\s+/) {
      $vsys{$1}=$2;
    }
  }
  return %vsys;
}

#get a value from the ns box
sub getValue {
  my ($self, $setting) = @_;
  return $self->error("No setting specified") unless $setting;
  my @result= $self->cmd("get ".$setting); 
  if ($self->lastline =~ /\$\$Ambigious command!!/) {
    return $self->error("Ambigious command");
  }
  return @result;
}

#set a value in ns box
sub setValue {
  my ($self,$setting, $value) = @_;
  return $self->error("No setting specified") unless $setting;
  return $self->error("No value specified") unless $value;

  my @results=$self->cmd("set ".$setting." ".$value);
  foreach my $result (@results) {
    if ($result =~ /\w+/) { return $self->error($result); }
  }
  return 1;
}

#------------------------------------------
# Overridden Methods
#------------------------------------------

#destructor!

sub DESTROY {
  my $self=shift;
  if ($self->current_vsys()) {
    $self->exit;
  }
  $self->exit;
}

#set the prompt to that of a ns 1000 box..
sub new {
    my $class = shift;

    # There's a new cmd_prompt in town.
    my $self = $class->SUPER::new(
       	prompt => '/[\w().-]*\(?([\w.-])?\)?\s*->\s*$/',
       #	prompt => '/[\w().-]*i\(?([\w.-])?\)?\s*->\s*$/',
	@_,			# user's additional arguments
    ) or return;

    *$self->{net_telnet_ns1000} = {
	last_prompt  => '',
        last_cmd     => '',
    };

    $self
} # end sub new

# The new prompt() stores the last matched prompt for later
# fun 'n amusement. You can access this string via $self->last_prompt.
#
# It also parses out any router errors and stores them in the
# correct place, where they can be acccessed/handled by the
# Net::Telnet error methods.
#
# No POD docs for prompt(); these changes should be transparent to
# the end-user.
sub prompt {
    my( $self, $prompt ) = @_;
    my( $prev, $stream );

    $stream  = ${*$self}{net_telnet_ns1000};
    $prev    = $self->SUPER::prompt;

    ## Parse args.
    if ( @_ == 2 ) {
        defined $prompt or $prompt = '';

        return $self->error('bad match operator: ',
                            "opening delimiter missing: $prompt")
            unless $prompt =~ m|^\s*/|;

	$self->SUPER::prompt($prompt);

    } elsif (@_ > 2) {
        return $self->error('usage: $obj->prompt($match_op)');
    }

    return $prev;
} # end sub prompt



sub cmd {
    my $self             = shift;
    my $ok               = 1;
    my $cmd;

    # Extract the command from arguments
    if ( @_ == 1 ) {
	$cmd = $_[0];
    } elsif ( @_ >= 2 ) {
	my @args = @_;
	while ( my ( $k, $v ) = splice @args, 0, 2 ) {
	    $cmd = $v if $k =~ /^-?[Ss]tring$/;
	}
    }

    $ {*$self}{net_telnet_ns1000}{last_cmd} = $cmd;

    my @output = $self->SUPER::cmd(@_);

    for ( my ($i, $lastline) = (0, '');
	  $i <= $#output;
	  $lastline = $output[$i++] ) {

	# This may have to be a pattern match instead.
	if ( $output[$i] =~ /^\s*\^-+/ ) {

	    if ( $output[$i] =~ /unknown keyword (\w+)$/ ) { # Typo & bad arg errors
		chomp $lastline;
		$self->error( join "\n",
			             "Last command and firewall error: ",
			             ( $self->last_prompt . $cmd ),
			             $lastline,
			             "Unknown Keyword:" . $1,
			    );
		splice @output, $i - 1, 3;

	    } else { # All other errors.
		chomp $output[$i];
		$self->error( join "\n",
			      "Last command and firewall error: ",
			      ( $self->last_prompt . $cmd ),
			      $output[$i],
			    );
		splice @output, $i, 2;
	    }

	    $ok = undef;
	    last;
	}
    }
    return wantarray ? @output : $ok;
}

sub waitfor {
    my $self = shift;
    return unless @_;

    # $isa_prompt will be built into a regex that matches all currently
    # valid prompts.
    #
    # -Match args will be added to this regex. The current prompt will
    # be appended when all -Matches have been exhausted.
    my $isa_prompt;

    # Things that /may/ be prompt regexps.
    my $promptish = '^\s*(?:/|m\s*\W).*';


    # Parse the -Match => '/prompt \$' type options
    # waitfor can accept more than one -Match argument, so we can't just
    # hashify the args.
    if ( @_ >= 2 ) {
	my @args = @_;
	while ( my ( $k, $v ) = splice @args, 0, 2 ) {
	    if ( $k =~ /^-?[Mm]atch$/ && $v =~ /($promptish)/ ) {
		if ( my $addme = re_sans_delims($1) ) {
		    $isa_prompt .= $isa_prompt ? "|$addme" : $addme;
		} else {
		    return $self->error("Bad regexp '$1' passed to waitfor().");
		}
	    }
	}
    } elsif ( @_ == 1 ) {
	# A single argument is always a match.
	if ( $_[0] =~ /($promptish)/ and my $addme = re_sans_delims($1) ) {
	    $isa_prompt .= $isa_prompt ? "|$addme" : $addme;
	} else {
	    return $self->error("Bad regexp '$_[0]' passed to waitfor().");
	}
    }


    # Add the current prompt if it's not already there.
    if ( index($isa_prompt, $self->prompt) != -1
	 and my $addme = re_sans_delims($self->prompt) ) {
	$isa_prompt .= "|$addme";
    }

    # Call the real waitfor.
    my ( $prematch, $match ) = $self->SUPER::waitfor(@_);

    # If waitfor was, in fact, passed a prompt then find and store it.
    if ( $isa_prompt && defined $match ) {
	(${*$self}{net_telnet_ns1000}{last_prompt})
	    = $match =~ /($isa_prompt)/;
    }
    return wantarray ? ( $prematch, $match ) : 1;
}


sub login {
    my($self) = @_;
    my(
       $cmd_prompt,
       $endtime,
       $error,
       $lastline,
       $match,
       $orig_errmode,
       $orig_timeout,
       $passwd,
       $prematch,
       $reset,
       $timeout,
       $usage,
       $username,
       %args,
       );
    local $_;

    ## Init vars.
    $timeout = $self->timeout;
    $self->timed_out('');
    return if $self->eof;
    $cmd_prompt = $self->prompt;
    $usage = 'usage: $obj->login(Name => $name, Password => $password, '
	   . '[Prompt => $match,] [Timeout => $secs,])';

    if (@_ == 3) {  # just username and passwd given
	($username, $passwd) = (@_[1,2]);
    }
    else {  # named args given
	## Get the named args.
	(undef, %args) = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?name$/i) {
		$username = $args{$_};
		defined($username)
		    or $username = "";
	    }
	    elsif (/^-?pass/i) {
		$passwd = $args{$_};
		defined($passwd)
		    or $passwd = "";
	    }
	    elsif (/^-?prompt$/i) {
		$cmd_prompt = $args{$_};
		defined $cmd_prompt
		    or $cmd_prompt = '';
		return $self->error("bad match operator: ",
				    "opening delimiter missing: $cmd_prompt")
		    unless ($cmd_prompt =~ m(^\s*/)
			    or $cmd_prompt =~ m(^\s*m\s*\W)
			   );
	    }
	    elsif (/^-?timeout$/i) {
		$timeout = _parse_timeout($args{$_});
	    }
	    else {
		return $self->error($usage);
	    }
	}
    }

    return $self->error($usage)
	unless defined($username) and defined($passwd);

    ## Override these user set-able values.
    $endtime = _endtime($timeout);
    $orig_timeout = $self->timeout($endtime);
    $orig_errmode = $self->errmode('return');

    ## Create a subroutine to reset to original values.
    $reset
	= sub {
	    $self->errmode($orig_errmode);
	    $self->timeout($orig_timeout);
	    1;
	};

    ## Create a subroutine to generate an error for user.
    $error
	= sub {
	    my($errmsg) = @_;

	    &$reset;
	    if ($self->timed_out) {
		return $self->error($errmsg);
	    }
	    elsif ($self->eof) {
		($lastline = $self->lastline) =~ s/\n+//;
		return $self->error($errmsg, ": ", $lastline);
	    }
	    else {
		return $self->error($self->errmsg);
	    }
	};

    ## Wait for login prompt.
    ($prematch, $match) = $self->waitfor(-match => '/[Ll]ogin[:\s]*$/',
					 -match => '/[Uu]sername[:\s]*$/',
					 -match => '/[Pp]assword[:\s]*$/')
	or do {
	    return &$error("read eof waiting for login or password prompt")
		if $self->eof;
	    return &$error("timed-out waiting for login or password prompt");
	};

    unless ( $match =~ /[Pp]ass/ ) {
	## Send login name.
	$self->print($username)
  	    or return &$error("login disconnected");

	## Wait for password prompt.
	$self->waitfor(-match => '/[Pp]assword[: ]*$/')
	    or do {
		return &$error("read eof waiting for password prompt")
		    if $self->eof;
		return &$error("timed-out waiting for password prompt");
	    };
    }
	
    ## Send password.
    $self->print($passwd)
        or return &$error("login disconnected");

    ## Wait for command prompt or another login prompt.
    ($prematch, $match) = $self->waitfor(-match => '/[Ll]ogin[:\s]*$/',
					 -match => '/[Uu]sername[:\s]*$/',
					 -match => '/[Pp]assword[:\s]*$/',
					 -match => $cmd_prompt)
	or do {
	    return &$error("read eof waiting for command prompt")
		if $self->eof;
	    return &$error("timed-out waiting for command prompt");
	};

    ## Reset object to orig values.
    &$reset;

    ## It's a bad login if we got another login prompt.
    return $self->error("login failed: access denied or bad name or password")
	if $match =~ /(?:[Ll]ogin|[Uu]sername|[Pp]assword)[: ]*$/;

    1;
} # end sub login

#------------------------------
# Class methods
#------------------------------

# Return a Net::Telnet regular expression without the delimiters.
sub re_sans_delims { ( $_[0] =~ m(^\s*m?\s*(\W)(.*)\1\s*$) )[1] }

# Look for subroutines in Net::Telnet if we can't find them here.
sub AUTOLOAD {
    my ($self) = @_;
    croak "$self is an [unexpected] object, aborting" if ref $self;
    $AUTOLOAD =~ s/.*::/Net::Telnet::/;
    goto &$AUTOLOAD;
}

=pod

=head1 NAME

Net::Telnet::Ns1000 - interact with a Netscreen-1000 firewall

=head1 SYNOPSIS

use Net::Telnet::Ns1000;

my $fw = new Net::Telnet::Ns1000(host=>'62.65.31.108');
$fw->login('admin','password') or die $fw->error;
$fw->enter_vsys('norway.com');
print "We are now in: ".$fw->current_vsys."\n";
my %vsys=$fw->get_vsys;
   foreach $key (sort (keys %vsys)) {
     print $key,'=', $vsys{$key},"\n";
   }
print @results;

=head1 DESCRIPTION

Net::Telnet::Ns1000 is mostly a pure rippoff of Net::Telnet::Cisco, with
adaptations to make it work on the Netscreen 1000 firewalls.
It also has some additional commands, but for basic functionality, 
see Net::Telnet and Net::Telnet::Cisco documentation.

=head1 FIRST

Before you use Net::Telnet::Ns1000, you should probably have a good
understanding of Net::Telnet, so perldoc Net::Telnet first, and then
come back to Net::Telnet::Ns1000 to see where the improvements are.

Some things are easier to accomplish with Net::SNMP. SNMP has three
advantages: it's faster, handles errors better, and doesn't use any
vtys on the router. SNMP does have some limitations, so for anything
you can't accomplish with SNMP, there's Net::Telnet::Ns1000.

=head1 METHODS

New methods not found in Net::Telnet follow:

=over 4

=item B<enter_vsys> - enter a virtual system

Enter a virtual system in the firewall.
parameter is system you want to enter ..
You may enter another vsys even if you are
in a vsys.. Note that we will save your changes
for you if you do.


=item B<enter_vsys> - exit from the level you are on

exit from the vsys you are in, or from the system
if you are on the top. takes one parameter...
if you should save any changes or not...

=item B<current_vsys> - show current vsys
returns blank if you're not in a vsys.

=item B<get_vsys> - return vsys.

returns a hash of all the virtual systems
on your system, with system id's for values

=item B<ping> - ping a system.
Returns percentage of success (0/100).

  $sucess=$fw->ping('192.168.1.1');

=item B<exit> - Exit system

use this command to exit system, or exit current
vsys

=item B<getValue> - Set a value from the box.
Will return a value from the firewall, or from
the vsys you are in, if you aren't in root.

=item B<setValue> - Set a Value in the box.
Set a value in the box, returns true if set successfully.
(guess what it returns if you fuck up? ;)

=back

=head1 AUTHOR

The basic functionality was ripped from
Joshua_Keroes@eli.net $Date: 2001/06/27 15:38:13 $
Modifications and additions to suit Netscreen was
done by
Marcus.Ramberg@songnetworks.no $Date: 2001/06/27 15:38:13 $

=head1 SEE ALSO

Net::Telnet, Net::SNMP

=head1 COPYRIGHT

Copyright (c) 2001 Marcus Ramberg, Song Networks Norway.
All rights reserved. This program is free software; you
can redistribute it and/or modify it under the same terms
as Perl itself.

=cut

1;

__END__
