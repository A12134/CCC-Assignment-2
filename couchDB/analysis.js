{
  "views": {
    "suburb": {
      "reduce": "_sum",
      "map": "function (doc) {\r\n    emit([doc.Period, doc.User_Location.address.city, doc.User_Location.address.suburb, \"Polarity\"], doc.Polarity);\r\n    if(doc.Polarity > 0){\r\n      emit([doc.Period, doc.User_Location.address.city, doc.User_Location.address.suburb, \"positiveCount\"],1);\r\n    }else if(doc.Polarity === 0){\r\n      emit([doc.Period, doc.User_Location.address.city, doc.User_Location.address.suburb, \"nuetralCount\"],1);\r\n    }else if(doc.Polarity < 0){\r\n      emit([doc.Period, doc.User_Location.address.city, doc.User_Location.address.suburb, \"negativeCount\"],1);\r\n    }\r\n}"
    }
  },
  "language": "javascript",
  "options": {
    "partitioned": false
  }
}
