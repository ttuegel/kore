pipeline {
    agent any
    stages {
        stage('Build - Java') {
            agent { docker { image 'maven:3-jdk-8' } }
            steps {
                sh '''
                    mvn clean
                    mvn verify
                '''
            }
        }
        stage('Build - Haskell') {
            agent { docker { image 'haskell:8.4' } }
            steps {
                sh '''
                    make STACK_OPTS="--test --bench --coverage" test-kore
                '''
            }
        }
        stage('Build - Haddock') {
            agent { docker { image 'haskell:8.4' } }
            steps {
                sh '''
                    make haddock
                '''
            }
        }
    }
}