package com.ilscipio.scipio.ce.util.logging;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;

import javax.sql.DataSource;

import org.apache.commons.dbcp2.ConnectionFactory;
import org.apache.commons.dbcp2.DriverManagerConnectionFactory;
import org.apache.commons.dbcp2.PoolableConnection;
import org.apache.commons.dbcp2.PoolableConnectionFactory;
import org.apache.commons.dbcp2.PoolingDataSource;
import org.apache.commons.pool2.impl.GenericObjectPool;

/**
 * SCIPIO: Error reporting, for use with log4j2.xml.
 * <p>
 * WARNING: This is currently mainly for Scipio internal use and is subject to change at any time.
 */
public class ScipioErrorDbConnectionFactory {
    private static final String DB_URL = "jdbc:mariadb://127.0.0.1/scipioerrordb";
    private static final String DB_USERNAME = "scipioerrordb";
    private static final String DB_PASSWORD = "scipio";

    private static interface Singleton {
        final ScipioErrorDbConnectionFactory INSTANCE = new ScipioErrorDbConnectionFactory();
    }

    private final DataSource dataSource;

    private ScipioErrorDbConnectionFactory() {
        Properties properties = new Properties();
        properties.setProperty("user", DB_USERNAME);
        properties.setProperty("password", DB_PASSWORD);
        ConnectionFactory connectionFactory = new DriverManagerConnectionFactory(DB_URL, properties);
        PoolableConnectionFactory poolableConnectionFactory = new PoolableConnectionFactory(connectionFactory, null);
        GenericObjectPool<PoolableConnection> connectionPool = new GenericObjectPool<>(poolableConnectionFactory);
        poolableConnectionFactory.setPool(connectionPool); // TODO: REVIEW: circular?
        poolableConnectionFactory.setValidationQuery("SELECT 1");
        poolableConnectionFactory.setValidationQueryTimeout(3);
        poolableConnectionFactory.setDefaultReadOnly(false);
        poolableConnectionFactory.setDefaultAutoCommit(false);
        poolableConnectionFactory.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
        // Original (log4j manual, dbcp lib):
        //PoolableConnectionFactory poolableConnectionFactory = new PoolableConnectionFactory(
        //        connectionFactory, connectionPool, null, "SELECT 1", 3, false, false, Connection.TRANSACTION_READ_COMMITTED
        //);
        this.dataSource = new PoolingDataSource<>(connectionPool);
    }

    public static Connection getConnection() throws SQLException {
        return Singleton.INSTANCE.dataSource.getConnection();
    }
}
