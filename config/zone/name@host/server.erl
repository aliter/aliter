% Host (node) for this server
{host, {host, name}}.

% Public IP/port configuration
{ip, {127,0,0,1}}.
{port, 6121}.

% Path to Aliter on this server's host
{aliter, "/path/to/aliter"}.

% Node of the character server associated with this zone server
{char, name@host}.

% Zone configuration for this server.
% A list of lists of maps. A list of maps = one zone.
{zones,
 [[prontera,
   prt_monk,
   prt_fild00,
   prt_fild01,
   prt_fild02,
   prt_fild03,
   prt_fild04,
   prt_fild05,
   prt_fild06,
   prt_fild07,
   prt_fild08,
   prt_fild09,
   prt_fild10,
   prt_fild11],
  [izlude],
  [geffen,
   gef_fild00,
   gef_fild01,
   gef_fild02,
   gef_fild03,
   gef_fild04,
   gef_fild05,
   gef_fild06,
   gef_fild07,
   gef_fild08,
   gef_fild09,
   gef_fild10,
   gef_fild11,
   gef_fild12,
   gef_fild13,
   gef_fild14],
  [payon,
   pay_arche,
   pay_fild01,
   pay_fild02,
   pay_fild03,
   pay_fild04,
   pay_fild05,
   pay_fild06,
   pay_fild07,
   pay_fild08,
   pay_fild09,
   pay_fild10,
   pay_fild11],
  [morroc,
   moc_fild01,
   moc_fild02,
   moc_fild03,
   moc_fild04,
   moc_fild05,
   moc_fild06,
   moc_fild07,
   moc_fild08,
   moc_fild09,
   moc_fild10,
   moc_fild11,
   moc_fild12,
   moc_fild13,
   moc_fild14,
   moc_fild15,
   moc_fild16,
   moc_fild17,
   moc_fild18,
   moc_fild19,
   moc_fild20,
   moc_fild21,
   moc_fild22,
   moc_ruins]]}.
