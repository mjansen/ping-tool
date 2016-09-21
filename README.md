# ping-tool

What I need is a tool that can ping a number of ip addresses to see what machines are up.  We assume here that all machines
can respond to ICMP ping requests, and that firewalls do not get in the way.

The main motivation is to be able to detect problems when changes to network configurations are made, or perhaps when hosts
are reconfigured.  We just want to notice any connectivity problems early.

A tool along this line is `MTR`.  However, `MTR` only deals with one ip address at a time, and is interested in packet loss at
intermediate router hops.  Another tool is `nmap`, but `nmap` does sophisticated penetration scans without easily monitoring
network connectivity changes.  I am looking for a tool that can monitor reachability (ICMP echo ok) of live hosts on complete
subnets.

So that is what we have set out to do.

The scanning frequency should be quite high (at least one ping per second).
