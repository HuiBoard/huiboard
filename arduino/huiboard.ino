// Copyright (c) 2016 Joshua Miller

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

unsigned long newtime;
unsigned long oldtime;
unsigned long time;
//Unlike standard longs unsigned longs won't store negative numbers, making their range from 0 to 4,294,967,295 (2^32 - 1).
char newstate[10] = {'0', '0', '0', '0', '0', '0', '0', '0', '0', '0'};
char oldstate[10] = {'0', '0', '0', '0', '0', '0', '0', '0', '0', '0'};

void setup() {
  for (int i=2; i <=12; i++){
    pinMode(i, INPUT);
  }
  Serial.begin(115200);
}

boolean arraycompare(char *a, char *b){
  for (int i=0; i<10; i++) {
    if (a[i] != b[i]) {
      return false;
    }
  }
  return true;
}

void loop() {
  for (int i=2; i < 12; i++) {
    if (digitalRead(i) == HIGH) {
      newstate[i-2] = '1';
    }
    else {
      newstate[i-2] = '0';      
    }
  }
  if (arraycompare(oldstate, newstate) == false) {
    Serial.write('[');
    Serial.print(newstate);
    newtime = millis();
    //if oldtime > newtime, than add to rollover
    time = newtime - oldtime;
    oldtime = newtime;
    Serial.write(',');
    Serial.print(time);
    Serial.write(']');
    Serial.write('\n');
    for (int i=0; i < 10; i++) {
      oldstate[i] = newstate[i];
    }
  } 
  //delay(10);
}
