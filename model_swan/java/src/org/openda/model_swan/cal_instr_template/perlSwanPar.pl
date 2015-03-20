#!/usr/bin/env perl

use strict;
use File::Copy;

open TESTLOGFILE,">perlSwanPar-log.txt";
print TESTLOGFILE "perl script is RUNNING\n";
print TESTLOGFILE "cwd()\n";

# Set environment
my $MPI_ROOT = '/opt/mpich2';
$ENV{MPI_ROOT} = $MPI_ROOT;
$ENV{MPI_LIBS} = "-L$MPI_ROOT/lib -lmpich -lpthread -lrt -i-static";
$ENV{MPI_RSH}  = '/usr/bin/rsh';

my $debug = 0;

# Get parameters
my ($swanfile,$mpi,$killmpd,$parallelSwanExe,$out);
while ( @ARGV ) {
   my $arg = shift;
   if ( $arg =~ /-input/ ) {
      $swanfile = shift;
      next;
      };
   if ( $arg =~ /-mpi/ ) {
      $mpi = shift;
      next;
      };
   if ( $arg =~ /-killmpd/ ) {
      $killmpd = shift;
      next;
      };
   if ( $arg =~ /-swanexe/ ) {
      $parallelSwanExe = shift;
      next;
      };
   if ( $arg =~ /-output/ ) {
      $out = shift;
      next;
      };
   print "ERROR: invalid argument: $arg\n";
   print TESTLOGFILE "ERROR: invalid argument: $arg\n";
   close TESTLOGFILE;
   exit 1;
}
#
#unless ( -f $swanfile ) {
#   print TESTLOGFILE "ERROR: swan input file $swanfile does not exist!\n";
#   close TESTLOGFILE;
#   die "ERROR: swan input file $swanfile does not exist!\n";
#}
unless ( -f "$ENV{SWANEXESDIR}/linuxPar/$parallelSwanExe" ) {
   print TESTLOGFILE "ERROR: swan parallel exe $parallelSwanExe does not exist!\n";
   close TESTLOGFILE;
   die "ERROR: swan parallel exe $parallelSwanExe does not exist!\n";
}
if ( $out and not -d $out ) {
   print TESTLOGFILE "ERROR: directory for output $out does not exist!\n";
   close TESTLOGFILE;
   die "ERROR: directory for output $out does not exist!\n";
}

# Copy the Swan inputfile to INPUT
#$swanfile .= '.swn' unless $swanfile =~ /\.swn/;
#unlink 'INPUT' if -f 'INPUT';
#copy $swanfile,'INPUT';

# Start Swan
my $status = startSwan ($debug,$mpi);
#
# in case of an error with MPI
#

print TESTLOGFILE "status: $status\n";
close TESTLOGFILE;

die "ERROR: mpiexec/mpirun terminated with status=$status\n" if $status;

$out = '.' unless -d $out;

if ( $mpi<2 ) {

   rename "PRINT"  ,"$out/$swanfile.prt" if -f "PRINT";
   rename "Errfile","$out/$swanfile.erf" if -f "Errfile";

} else {
   for ( my $i0=0; $i0<$mpi; $i0++ ) {
      my $i1 = sprintf("%3.3d",$i0+1);
      mv ("PRINT-$i1"  ,"$out/$swanfile.prt-$i1");
      mv ("Errfile-$i1","$out/$swanfile.erf-$i1");
      mv ("log$i0.irlog","$out/log$i0.irlog") unless $out eq '.';
   }
}
rename "ERRPTS","$out/$swanfile.erp" if -f "ERRPTS";

`cat norm_end`                  if -f "norm_end";
unlink "machinefile"            if -f "machinefile";
#unlink "INPUT";
unlink "swaninit";

exit $status;

#---------------------------------------------------------------------------
sub MakeHostfile
{
use Sys::Hostname;
#
#  ($hostfile, \@hosts, \%ppn) = MakeHostfile( $idebug )
#
#  Prepare a hostfile for starting MPI, using the hosts assigned to us by
#  PBS (PBS_NODEFILE), a user-provided hostfile, or the default hostfile.gen
#  Return the names of the hosts and the number of processors per host.
#
#------------------------------------------------------------------
#  INPUT ARGUMENTS
#
   my $idebug     = shift;
#
#------------------------------------------------------------------
#  OUTPUT ARGUMENTS
#
   my $hostfile = "/tmp/hostfile$$";  
                # name of the hostfile for starting MPI
   my @hosts;   # list of host-names used by the run
   my %ppn;     # processors per node/host

   # In case the job is started by PBS (portable batch system) or SGE:
   # read list of hosts for the configuration from file $PBS_NODESFILE or
   # $DELTAQ_Nodelist, respectively.

   my @all_hosts;
   if (exists $ENV{DELTAQ_NodeList}) {
      # read list of hosts assigned to us by SGE
      @all_hosts = split m/ /, $ENV{DELTAQ_NodeList};

      # copy unique hostnames of @all_hosts to @hosts;
      # count number of occurrences per host
      foreach my $host (@all_hosts) {
         if (not defined $ppn{$host}) {
            push(@hosts, $host);
         };
         $ppn{$host}++;      # increment number of occurrences of 'host'
      }
   } elsif ($ENV{PBS_NODEFILE} ne "" and -f $ENV{PBS_NODEFILE}) {
      # read list of hosts assigned to us by PBS
      open PBS_NODEFILE,"<$ENV{PBS_NODEFILE}";
      @all_hosts = <PBS_NODEFILE>; chomp(@all_hosts);
      close PBS_NODEFILE;

      # copy unique hostnames of @all_hosts to @hosts;
      # count number of occurrences per host
      foreach my $host (@all_hosts) {
         if (not defined $ppn{$host}) {
            push(@hosts, $host);
         };
         $ppn{$host}++;      # increment number of occurrences of 'host'
      }
   } else {
      # add current host to list all_hosts, must appear first in hostfile:
      my $exechost = hostname;
      if ($idebug >= 1) { print "running at host $exechost\n"; }
      push(@hosts, $exechost);
      $ppn{$exechost} = 1;
      @all_hosts = @hosts;

      # If user has not supplied hostfile.gen then use default version.
      #lnfiles("hostfile.gen");

      # retrieve all non-comment lines not containing $host from
      # hostfile.gen to list of hosts
      my $check_1st_host = 1;
      #open HOSTGEN,"<$si_workdir/hostfile.gen";
      #foreach my $line (<HOSTGEN>) {
      #   chomp($line);
      #   # line is something like: "  vortech25  ppn =2  # commentaar"
      #   # pattern: (hostname)(?:remainder)? -> optional remainder, not captured
      #   # remainder: "ppn=" plus whitespace, digits (captured)
      #   my ($host, $ppn) = $line =~ m/^\s*([^#\s]+)(?:\s*ppn\s*=\s*(\d+))?/i ;
      #   # print "next line: obtained host=\"$host\", ppn=$ppn\n";
      #   if (not defined $ppn) { $ppn = 1; }
      #   if (defined $host and $check_1st_host and $host ne $exechost ) {
      #      print "\nWARNING: the execution host \"$exechost\" where the run ".
      #            "is started is not\n         the first host in your ".
      #            "hostfile. It will be made the first host.\n         This ".
      #            "affects the mapping of processes onto hosts.\n\n";
      #   }
      #   $check_1st_host = 0;
      #   if ( $host eq $exechost ) {
      #      $ppn{$exechost} = $ppn;
      #   } elsif ( defined $host ) {
      #      push (@hosts, $host);
      #      $ppn{$host} = $ppn;
      #   }
      #};
      #close HOSTGEN;
   };

   # print list of hosts to hostfile
   open HOSTFILE,">$hostfile";
   #foreach my $host (@hosts) { print HOSTFILE "$host cpu=$ppn{$host}\n"; }
   foreach my $host (@all_hosts) { print HOSTFILE "$host\n"; }
   close HOSTFILE;

   # make sure no stale mpd proces exists
   if ($killmpd ne "") {
     foreach my $host (@all_hosts) {
       print "KILLMPD on $host";
       system("rsh $host $killmpd");
       # use a 2nd method if the mpi provided tool doesnt work
       system("rsh /usr/bin/pkill -u $ENV{USER} -f mpd");
     }
   }

   # print hostfile used for debugging purposes
   if ($idebug >= 2) {
      print "Contents of $hostfile used for starting MPI:\n";
      open HOSTFILE,"<$hostfile";
      foreach my $line (<HOSTFILE>) { print $line };
      close HOSTFILE;
   }

   # return name of hostfile, list of hosts and number of processors per host
   return ($hostfile, \@hosts, \%ppn);
};
# end sub MakeHostfile

#---------------------------------------------------------------------------
sub MapProcesses
{
#
#  ( @hostmap ) = MapProcesses( $idebug, $num_prc, \@hosts, \%ppn,
#                                                           $userhostmap )
#
#  Determine mapping of processes to hosts using user-supplied mapping or
#  using Round Robin mapping method.
#
#------------------------------------------------------------------
#  INPUT ARGUMENTS
#
   my $idebug      = shift;
   my $num_prc     = shift; # number of Waqpro processes
   my $hosts_ptr   = shift; # pointer to list of hosts
   my $ppn_ptr     = shift; # pointer to number of processors per host
   my $userhostmap = shift; # user-provided host-mapping (default is
                            # "Round-Robin", otherwise a comma-separated
                            # list of values in [1..nhosts])
#
#------------------------------------------------------------------
#  OUTPUT ARGUMENTS
#
   my @hostmap;  # for each process to be started the corresponding index
                 # in the list of hosts.

   # obtain actual list of hosts and hash with processors per host by
   # dereferencing pointers
   my @hosts     = @$hosts_ptr;
   my %ppn       = %$ppn_ptr;
   my $num_hosts = $#hosts + 1;

   # determine maximum value of ppn over all hosts;
   my $max_ppn   = 1;
   foreach my $ihost ( 0 .. $num_hosts-1 ) {
      if ($ppn{$hosts[$ihost]} > $max_ppn) { $max_ppn = $ppn{$hosts[$ihost]} };
   }

   # print available hosts and ppn when requested
   if ($idebug >= 2) { 
      print "Number of hosts=$num_hosts:";
      foreach my $ihost ( 0 .. $num_hosts-1 ) {
         if ( $ppn{$hosts[$ihost]} > 1 ) {
            print " $hosts[$ihost] (ppn=$ppn{$hosts[$ihost]})"; 
         } else {
            print " $hosts[$ihost]";
         }
      }
      print "\n";
   }

   # Validate user-defined mapping first; if it fails revert to Round-Robin
   if (not (uc($userhostmap) eq "ROUND-ROBIN")) {
      if ($idebug >= 2) { 
         print "User-defined mapping: \"-hostmap $userhostmap\".\n"
      }
      my @userhostmap=split(',',$userhostmap);

      # Catch non-numeric or out-of-range values first
      my $ierror = 0;
      foreach my $ihost (@userhostmap) {
         if (not $ierror and not ($ihost > 0 and $ihost <= $num_hosts)) {
            print "\nWARNING: value \"$ihost\" in hostmap is not numeric or out of range [1-$num_hosts];\n".
                  "         reverting to Round-Robin mapping method.\n\n";
            $ierror++;
         }
      }
      # Check number of values provided by user
      if (not $ierror and ($#userhostmap+1 != $num_prc)) {
         print "\nWARNING: hostmap does not contain $num_prc values;\n".
               "         reverting to Round-Robin mapping method.\n\n";
         $ierror++;
      }
      # If argument is ok: convert into interal hostmap (0-based)
      if (not $ierror) {
         foreach my $iprc ( 1 .. $num_prc ) {
            my $ihost = $userhostmap[ $iprc-1 ] - 1; # convert to 0-based value
            push(@hostmap, $ihost);
            if ($idebug >= 0) { printf "Mapping Swan[%d] onto host %d (%s)\n",
                                    $iprc, $ihost+1, $hosts[$ihost]; }
         }
      }
   }

   # Round-Robin mapping method.
   if (not defined @hostmap) {
      if ($idebug >= 2) { 
         print "Using Round-Robin mapping of $num_prc processes onto $num_hosts hosts\n"; 
      }
      # First all hosts get one process; then all hosts with ppn>=2 get a
      # second process, and so on upto ppn=max_ppn; then the loop is started
      # over again at ppn=1.
      my $ihost = 0;
      my $find_ppn = 1;
      if ($idebug >= 4) { print "First candidate: host $ihost ($hosts[$ihost],".
                        " ppn=$ppn{$hosts[$ihost]}), find_ppn=$find_ppn\n"; }
      foreach my $iprc ( 1 .. $num_prc ) {
         # map Swan subdomain iprc to host ihost
         push(@hostmap, $ihost);
         if ($idebug >= 0) { printf "Mapping Swan[%d] onto host %d (%s)\n",
                                 $iprc-1, $ihost+1, $hosts[$ihost]; }
         # search next host with ppn>=find_ppn
         my $found=0;
         while (not $found) {
            $ihost++; 
            if ($ihost >= $num_hosts) { $find_ppn++; $ihost = 0 }
            if ($find_ppn > $max_ppn) { $find_ppn = 1 }
            if ($idebug >= 4) { print "Next candidate: host $ihost ".
               "($hosts[$ihost], ppn=$ppn{$hosts[$ihost]}), ".
               "find_ppn=$find_ppn\n"; }
            if ($ppn{$hosts[$ihost]} >= $find_ppn) { $found = 1 }
         }
      }
   }

   # return mapping of processes onto hosts
   if ($idebug >= 3) {
      print "Mapping: host-numbers per process (0-based values): @hostmap\n";
   }
   return ( @hostmap );
};
# end sub MapProcesses
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
sub startSwan
{
use Cwd;
use Sys::Hostname;
#
#    $expect_success = startSwan( $idebug, $num_prc );
#
#  Carry out the processing phase of the simulation, in which the
#  SWAN processes carry out time steps
#
#------------------------------------------------------------------
#
#  INPUT ARGUMENTS
#
   my $idebug      = shift;
   my $num_prc     = shift;

#---------------------------------------------------------------------
#
#  INPUT/OUTPUT ARGUMENTS
#
   my $expect_success = 0;
#
#---------------------------------------------------------------------
   # get current directory, strip leading directories when appropriate
   my $pwd = cwd();
   unlink ('norm_end') if -f 'norm_end';

   if ( $num_prc > 1 ) {
      # Prepare a hostfile for starting MPI, using the hosts assigned to
      # us by PBS, a user-provided hostfile, or the default hostfile.gen

      my ($hostfile, $hosts_ptr, $ppn_ptr) = MakeHostfile( $idebug );
      my @hosts = @$hosts_ptr; 

      # Determine mapping of processes to hosts
      my $userhostmap = "ROUND-ROBIN";
      my @hostmap = MapProcesses( $idebug, $num_prc, $hosts_ptr, $ppn_ptr,
                                  $userhostmap );

      # to boot an mpd parallel environment, use the remote shell
      # (rsh) unless otherwise specified in Settings.inc
      # The secure shell (ssh) is usually the default for mpd, but
      # the rsh is retained for backward compatibility reasons.
      my $rsh_str = '-r rsh ';
      if (defined $ENV{MPI_RSH}) {
         $rsh_str = '-r ' . $ENV{MPI_RSH};
      }

      # Check whether mpd is running already; if not: start mpd.
      # If mpd is running already, make sure they are kept after
      # waqpro finishes
      my $mpd_was_running;
      system("$MPI_ROOT/bin/mpdtrace > /dev/null");
      my $status = $?;
      if ( $status == 0 ) {
         $mpd_was_running = 1;
         print("Note: MPI is already booted, using previous configuration of hosts\n",
               "      MPI daemons will be kept after WAQPRO finishes.\n");
      } else {
         $mpd_was_running = 0;
         my $num_hosts=$#hosts + 1 ;
         my $cmd = "$MPI_ROOT/bin/mpdboot -n $num_hosts " .
                   "-f $hostfile $rsh_str";
         if ($idebug >= 2) { print("booting MPI with command:\n$cmd\n"); }
         system($cmd);
      }

      # Start SWAN
      my $swanexe = `which $parallelSwanExe`;
      chomp $swanexe;
      open LU,">/tmp/procgroup$$";
      foreach my $iprc ( 0 .. $num_prc-1 ) {
         my $ihst = $hostmap[$iprc];
         my $host = $hosts[$ihst];
         print LU "-host $host -n 1 -wdir $pwd $swanexe\n";
      }
      close LU;
      my $cmd = "$MPI_ROOT/bin/mpiexec -configfile /tmp/procgroup$$";
      my $ihst = $hostmap[0];
      my $host = $hosts[$ihst];
      #my $cmd = "$MPI_ROOT/bin/mpiexec -machinefile $hostfile -n $num_prc $swanexe";
      #my $cmd = "$MPI_ROOT/bin/mpiexec -n $num_prc -wdir $pwd $swanexe";
   
      #my $cmd = "mpirun -np $num_prc $swanexe";
      #my $cmd = "mpirun -np $num_prc $hostfile $swanexe";
      if ($idebug >= 2) {
         print("starting parallel run with command:\n$cmd\n");
      }
      system($cmd);
      unlink "/tmp/procgroup$$";
      unlink $hostfile;

      # if mpd was running before waqpro.pl was run, keep daemons
      if ($mpd_was_running) {
         print "\n\n--- keeping mpd-daemons because they were started before waqpro...\n";
         print "    use 'mpdallexit' to stop mpd-daemons manually.\n";
      }
      else {
         # Determine if there are MPI-jobs still running;
         # if not: shutdown mpd.
         my $listjobs =`$MPI_ROOT/bin/mpdlistjobs`;
         if ( $listjobs eq "") {
            system("$MPI_ROOT/bin/mpdallexit");
            print "\n\n--- all mpd-daemons stopped...\n";
         }
         else {
            print "\n\n--- mpd-daemons were not stopped because processes were running.\n";
            print "    use 'mpdallexit' to stop mpd-daemons manually.\n";
         }
      } # end if $mpd_was_running

   # sequential run
   } else {
      my $swanexe = `which swan.exe`;
      chomp $swanexe;
      system($swanexe);
   }

   #
   # in case of an error: write message at end of messagefile
   #
   my $statexec=not -f 'norm_end';
   if ( $statexec )
   {
      warn "**************************************************************************\n".
           "*** ERROR: mpiexec terminated with status = $statexec\n".
           "**************************************************************************\n";
      $expect_success++;
   };
   return $expect_success;
};
sub mv {
   my ($from,$to) = @_;
   unlink $to       if -f $to;
   rename $from,$to if -f $from;
}
# end sub startSwan
