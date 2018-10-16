import urllib.request
import urllib.parse
from urllib import request, parse

response = urllib.request.urlopen('https://www.python.org')
## print(response.read().decode('utf-8'))    ## 用utf-8编码打印response

print(type(response))

print(response.status)                       ## 获取状态码
print(response.getheaders())                 ## 获取响应头信息
print(response.getheader('Server'))          ## 获取响应头中Server的值

data = bytes(urllib.parse.urlencode({'word': 'hello'}), encoding='utf8')
response = urllib.request.urlopen('http://httpbin.org/post',data=data)
print(response.read())

## response = urllib.request.urlopen('http://httpbin.org/get',timeout=1)
print(response.read())

url = 'http://httpbin.org/post'

headers = {
    'User-Agent': 'Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)',
    'Host': 'httpbin.org'
}

dict={
    'name': 'Germey'
}

data = bytes(parse.urlencode(dict), encoding='utf8')
req = request.Request(url=url, data=data, headers=headers, method='POST')
response = request.urlopen(req)
print(response.read().decode('utf-8'))
