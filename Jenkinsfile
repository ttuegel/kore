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
            agent { docker { image 'samdoshi/haskell-stack' } }
            steps {
                sh '''
                    make STACK_OPTS="--test --bench --coverage" test-kore
                '''
            }
        }
        stage('Build - Haddock') {
            agent { docker { image 'samdoshi/haskell-stack' } }
            steps {
                sh '''
                    make haddock
                '''
            }
        }
    }
}