"use strict";

import winston from 'winston';
import 'winston-daily-rotate-file';

const { combine, timestamp, json } = winston.format;

function fileRotateTransport (dir) {
  return new winston.transports.DailyRotateFile({
    filename: (dir + '/tunebank-%DATE%.log'),
    datePattern: 'YYYY-MM-DD',
    maxFiles: '14d',
  });
};

export function createLoggerImpl (dir) {
  var transport = fileRotateTransport (dir);
  return winston.createLogger({
    level: 'info',
    format: combine(timestamp(), json()),
    transports: [transport],
  })
};

export function logErrorImpl (logger, message) {
  return logger.error (message);
};

export function logInfoImpl (logger, message) {
  return logger.info (message);
};

export function logWarnImpl (logger, message) {
  return logger.warn (message);
};
