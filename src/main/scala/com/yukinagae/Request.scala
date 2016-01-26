package com.yukinagae

case class Request(//
  serverPort: Int,//
  serverName: String,//
  remoteAddr: String,//
  URI: String,//
  queryString: String,//
  scheme: String,//
  requestMethod: String,//
  protocol: String,//
  headers: Map[String, String],//
  contentType: String,//
  contentLength: Int,//
  characterEncoding: String,//
  // sslClientCert: String,//
  body: String//
)
