firstreq = 1
nextreq resp = -resp + 1
process req = -( req + 1 )

client init ~(resp:resps) = init : client (nextreq resp ) resps
server (req:reqs) = process req : server reqs

reqs = client firstreq resps
resps = server reqs



