#!/usr/bin/perl

use WordPress::API::Post;

my $p = WordPress::API::Post->new({
    username => 'rodney',
    password => '',
    proxy => 'http://rodney.id.au/xmlrpc.php',
				  });   

$title = <STDIN>;
$date = <STDIN>;
@post = <STDIN>;
@categories = [ 'tour' ];

$p->title($title);
$p->dateCreated($date);
$p->description(join('', @post));
$p->categories(@categories);
   
$p->save; # save to wordpress

$p->id; # what is the id for this?   
$p->url; # how would we access this page via http?
