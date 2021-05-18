{
  "views": {
      "city": {
          "reduce": "_sum",
          "map": "function (doc) {\r\n  if(doc.User_Location_Type === \"city\" && doc.User_Location !== \"\"){\r\n    emit([doc.Period, doc.User_Location, \"Polarity\"], doc.Polarity);\r\n    if(doc.Polarity > 0){\r\n      emit([doc.Period, doc.User_Location, \"positiveCount\"],1);\r\n    }else if(doc.Polarity === 0){\r\n    emit([doc.Period, doc.User_Location, \"nuetralCount\"],1);\r\n    }else if(doc.Polarity < 0){\r\n      emit([doc.Period, doc.User_Location, \"negativeCount\"],1);\r\n    }\r\n  } else if(doc.User_location_Type === \"city\" && doc.User_location !== \"\"){\r\n    emit([doc.Period, doc.User_location, \"Polarity\"], doc.Polarity);\r\n    if(doc.Polarity > 0){\r\n      emit([doc.Period, doc.User_location, \"positiveCount\"],1);\r\n    }else if(doc.Polarity === 0){\r\n      emit([doc.Period, doc.User_location, \"nuetralCount\"],1);\r\n    }else if(doc.Polarity < 0){\r\n      emit([doc.Period, doc.User_location, \"negativeCount\"],1);\r\n    }\r\n  }\r\n}"
      }
  },
  "language": "javascript",
  "options": {
      "partitioned": false
  }
}
