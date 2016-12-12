import requests
import json

portnum = '8081'

# =====================================================
#	IdentityServer "submit" - Success
# =====================================================
dstr = 'DirectoryServer'
r = requests.post('http://localhost:8081/submit',
 data = {'address':'localhost', 'port' : '0', 'serverType' : dstr})
 
assert r.status_code == 200

result = json.dumps(r.text)

assert result['code'] == 'IdentitySubmitSuccess'

assert result['server'] == {'address':'localhost', 'port' : portnum, 'serverType' : 'IdentityServer'}

# =====================================================
#	IdentityServer "submit" - Failure
# =====================================================

r = requests.post('http://localhost:8081/submit',
 data = {'address':'localhost', 'port' : '0', 'serverType' : 'DirectoryServer'})
 
assert r.status_code == 200

result = json.dumps(r.text)

assert result['code'] == 'IdentitySubmitFailure'

assert result['server'] == {'address':'localhost', 'port' : portnum, 'serverType' : 'IdentityServer'}

# =====================================================
#	IdentityServer "port" - Success
# =====================================================

port = 0
r = requests.get('http://localhost:8081/port/{1}'.format(dstr))
 
assert r.status_code == 200

result = json.dumps(r.text)

assert result['code'] == 'IdentityFound'

assert result['payload'] == '8082'

port = result

# =====================================================
#	IdentityServer "port" - Failure
# =====================================================
fstr = 'FileServer'
r = requests.get('http://localhost:8081/port/{1}'.format(fstr))
 
assert r.status_code == 200

result = json.dumps(r.text)

assert result['code'] == 'IdentityNotFound'

# =====================================================
#	IdentityServer "next" - Success
# =====================================================

r = requests.get('http://localhost:8081/next/{1}'.format(dstr))
 
assert r.status_code == 200

result = json.dumps(r.text)

assert result['address'] == 'localhost'

assert result['port'] == port

assert result['serverType'] == dstr

# =====================================================
#	IdentityServer "next" - Failure
# =====================================================

r = requests.get('http://localhost:8081/next/{1}'.format(fstr))
 
assert r.status_code == 200

result = json.dumps(r.text)

assert result['address'] == ''

assert result['port'] == ''

assert result['serverType'] == ''

# =====================================================
#	IdentityServer "all" - Success
# =====================================================

r = requests.get('http://localhost:8081/all/{1}'.format(dstr))
 
assert r.status_code == 200

result = json.dumps(r.text)

assert len(result) == 1

assert result[0]['address'] == 'localhost'

assert result[0]['port'] == port

assert result[0]['serverType'] == dstr

# =====================================================
#	IdentityServer "all" - Failure
# =====================================================

r = requests.get('http://localhost:8081/next/{1}'.format(fstr))
 
assert r.status_code == 200

result = json.dumps(r.text)

assert len(result) == 0








