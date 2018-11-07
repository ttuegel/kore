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
    }
}